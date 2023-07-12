{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Conversation where

-- {{{1 import
import ClassyPrelude
import Data.Proxy
import Data.Kind (Type)

import Control.Monad.State.Strict hiding (mapM_)
import Control.Monad.Except hiding (mapM_)
import Control.Monad.Trans.Maybe
import Data.Conduit
import Control.Monad.Logger
import qualified Data.Conduit.List          as CL
import qualified Data.Text                  as T

import Text.Parsec.Text
import Text.Parsec.Error
import Text.Parsec.Pos                      (initialPos)
import Text.Parsec.Prim
import Data.Aeson.TH                        (deriveJSON, defaultOptions)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.InMsgHandler

import Yesod.Compat
-- }}}1

class TalkerState a where
    talkPromptNext :: a -> (Maybe Text, a)

    talkNotUnderstanding :: a -> ParseError -> (Maybe Text, a)

    -- | 解释出 Nothing 代表对话结束
    talkParser :: a -> Parser (Maybe Text, a)

    talkDone :: a -> Bool


data SomeTalkerState = forall a. TalkerState a => SomeTalkerState a

instance TalkerState SomeTalkerState where
    talkPromptNext (SomeTalkerState x)          = second SomeTalkerState $ talkPromptNext x
    talkNotUnderstanding (SomeTalkerState x)    = second SomeTalkerState . talkNotUnderstanding x
    talkParser (SomeTalkerState x)              = second SomeTalkerState <$> talkParser x
    talkDone (SomeTalkerState x)                = talkDone x


talkerRun :: (Monad m, TalkerState a) =>
    (m a)
    -> (a -> m ())
    -> ConduitC Text m Text
talkerRun = talkerRun' False

talkerRun' :: (Monad m, TalkerState a) =>
    Bool
    -> (m a)
    -> (a -> m ())
    -> ConduitC Text m Text
talkerRun' skip_first_prompt get_state put_state =
    if skip_first_prompt
        then chk_wait_and_go
        else go
    where
        prompt = do
            st <- lift get_state
            let (m_t, s) = talkPromptNext st
            maybe (return ()) yield m_t
            lift $ put_state s

        go = prompt >> chk_wait_and_go

        chk_wait_and_go = do
            done <- liftM talkDone $ lift get_state
            if done
                then return ()
                else wait_and_go

        wait_and_go = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just t  -> do
                    st <- lift get_state
                    case parse (talkParser st) "" t of
                        Left err -> do
                            let (m_t, ms) = talkNotUnderstanding st err
                            maybe (return ()) yield m_t
                            lift $ put_state ms

                        Right (m_reply, new_st) -> do
                            maybe (return ()) yield m_reply
                            lift $ put_state new_st
                    go


talkerStateRunner :: (Monad m, TalkerState a) =>
    ConduitC Text (StateT a m) Text
talkerStateRunner = talkerRun get put


-- | 为Yesod应用而设计
-- 实际程序中，会话是通过无状态的连接实现的，所以需要一种保存状态的接口
-- 这个函数负责处理一次用户输入
conversationInputOneStep :: (Monad m, TalkerState s) =>
    m s                 -- ^ get state
    -> (s -> m ())      -- ^ set state
    -> Maybe Text
        -- ^ 新建立的会话时，用户输入理解为 Nothing
        -- 之后的调用都应该是 Just
    -> m [Text]
        -- ^ 状态机的输出文字
conversationInputOneStep get_st set_st m_input = do
    let send_intput = case m_input of
                        Nothing -> return ()
                        Just x -> yield x
    let cond = talkerRun' (isJust m_input) get_st set_st
    runConduit $ send_intput .| cond .| CL.consume


type WxTalkerMonad r m = ReaderT r (ExceptT String m)

mkWxTalkerMonad :: Monad m => (r -> m (Either String a)) -> WxTalkerMonad r m a
mkWxTalkerMonad f = do
    env <- ask
    lift $ ExceptT $ f env

runWxTalkerMonad :: WxTalkerMonad r m a -> r -> m (Either String a)
runWxTalkerMonad f env = runExceptT $ runReaderT f env

runWxTalkerMonadE :: WxTalkerMonad r m a -> r -> ExceptT String m a
runWxTalkerMonadE = runReaderT


-- | 支持更多操作的接口: 例如可以使用 IO 之类
-- 处理的用户输入也不仅仅是文字输入
class WxTalkerState r m a where
    -- | 为用户下一个输入提供提示
    -- 注意：这个方法会在 wxTalkHandleInput 之后调用
    wxTalkPromptToInput :: (WxTalkerMonad r m (Maybe a), a -> WxTalkerMonad r m ())
                        -> a
                        -> WxTalkerMonad r m ([WxppOutMsg], a)
                        -- ^ 回应消息及新的状态

    -- | 处理用户的输入，并产生结果
    -- 在会话过程中，消息可能会被无条件传入这个函数处理
    -- 要表达这个函数是否真正有处理用户输入，因此结果是个 Maybe
    wxTalkHandleInput :: (WxTalkerMonad r m (Maybe a), a -> WxTalkerMonad r m ())
                      -> a
                      -> WxppInMsgEntity
                      -> WxTalkerMonad r m ([Maybe WxppOutMsg], a)

    -- | 判断对话是否已结束
    wxTalkIfDone :: a -> WxTalkerMonad r m Bool



-- | 代表一种可以从微信消息创建的会话
class Monad m => WxTalkerFreshState r m a where
    -- | 新建一个会话
    -- Left 代表不能创建状态时，应返回的消息
    wxTalkInitiate :: WxppInMsgEntity -> WxTalkerMonad r m (Either [WxppOutMsg] a)

    -- | 用于无任何额外信息的前提下的初始化
    -- 例如程序自动初始化
    wxTalkInitiateBlank :: Proxy a -> WxppOpenID -> WxTalkerMonad r m (Either [WxppOutMsg] a)
    wxTalkInitiateBlank _ _ = return $ Left []


-- | 对应于会话正常结束的操作
-- 例如可保存会话中的临时数据等
class WxTalkerDoneAction r m a where
    wxTalkDone :: a -> WxTalkerMonad r m [WxppOutMsg]


-- | 会话中止的原因
data WxTalkAbortInitiator = WxTalkAbortByUser
                          | WxTalkAbortBySys
                          deriving (Show, Eq, Ord, Enum, Bounded)

-- | 对应于会话中止结束的操作
-- 中止通常由于会话超时，或用户使用＂中止＂指令
class WxTalkerAbortAction r m a where
  wxTalkAbort :: a -> WxTalkAbortInitiator -> WxTalkerMonad r m [WxppOutMsg]


wxTalkerRun :: (Monad m, Eq a, WxTalkerState r m a)
            => Bool
            -> (WxTalkerMonad r m (Maybe a))
            -> (a -> WxTalkerMonad r m ())
            -> ConduitC WxppInMsgEntity (WxTalkerMonad r m) (Maybe WxppOutMsg)
-- {{{1
wxTalkerRun skip_first_prompt get_state put_state =
    if skip_first_prompt
        then chk_wait_and_go
        else go
    where
        prompt = do
            m_st <- lift $ get_state
            case m_st of
                Nothing -> return ()
                Just st -> do
                    (replies, new_st) <- lift $ wxTalkPromptToInput (get_state, put_state) st
                    mapM_ yield $ map Just replies
                    unless (st == new_st) $ do
                        lift $ put_state new_st

        go = prompt >> chk_wait_and_go

        chk_wait_and_go = do
            done <- lift $ get_state >>= maybe (return True) wxTalkIfDone
            if done
                then return ()
                else wait_and_go

        wait_and_go = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just t  -> do
                    m_st <- lift $ get_state
                    case m_st of
                        Nothing -> return ()
                        Just st -> do
                            (replies, new_st) <- lift $ wxTalkHandleInput (get_state, put_state) st t
                            if null replies
                                then do
                                    -- 约定：如果 wxTalkHandleInput 返回空的消息列表，代表它拒绝处理这个输入消息
                                    wait_and_go
                                else do
                                    mapM_ yield replies
                                    unless (st == new_st) $ do
                                        lift $ put_state new_st
                                    go
-- }}}1


wxTalkerInputOneStep :: (Monad m, WxTalkerState r m s, Eq s) =>
    WxTalkerMonad r m (Maybe s)         -- ^ get state
    -> (s -> WxTalkerMonad r m ())      -- ^ set state
    -> r
    -> Maybe WxppInMsgEntity
        -- ^ 新建立的会话时，用户输入理解为 Nothing
        -- 之后的调用都应该是 Just
    -> m (Either String [Maybe WxppOutMsg])
        -- ^ 状态机的输出文字
-- {{{1
wxTalkerInputOneStep get_st set_st env m_input = flip runWxTalkerMonad env $ do
    runConduit $ send_intput .| cond .| CL.consume
    where
        send_intput = case m_input of
                        Nothing -> return ()
                        Just x -> yield x
        cond = wxTalkerRun (isJust m_input) get_st set_st
-- }}}1


-- | used to implement processInMsg of class IsWxppInMsgProcessor
-- 用于处理会话建立后的消息处理
wxTalkerInputProcessInMsg :: forall m r s .
    (MonadIO m, Eq s
    , WxTalkerState r m s
    , WxTalkerDoneAction r m s
    ) =>
    (WxppOpenID -> WxTalkerMonad r m (Maybe s))     -- ^ get state
    -> (WxppOpenID -> s -> WxTalkerMonad r m ())    -- ^ set state
    -> r
    -> Maybe WxppInMsgEntity
        -- ^ this is nothing only if caller cannot parse the message
    -> m (Either String WxppInMsgHandlerResult)
-- {{{1
wxTalkerInputProcessInMsg get_st set_st env m_ime = runExceptT $ do
    liftM (fromMaybe []) $ runMaybeT $ do
        ime <- MaybeT $ return m_ime
        let open_id = wxppInFromUserName ime
        (get_st', set_st') <- ioCachedGetSet (get_st open_id) (set_st open_id)
        st <- MaybeT $ run_wx_monad $ get_st'
        done <- lift $ run_wx_monad $ wxTalkIfDone st
        if done
            then do
                -- not in conversation
                return []

            else do
                msgs <- lift $ ExceptT $ wxTalkerInputOneStep get_st' set_st' env m_ime
                msgs2 <- if null msgs
                            then return []
                            else do
                                lift $ liftM (map Just . fromMaybe []) $ runMaybeT $ do
                                    new_st <- MaybeT $ run_wx_monad $ get_st'
                                    done2 <- lift $ run_wx_monad $ wxTalkIfDone new_st
                                    if done2
                                        then lift $ run_wx_monad $ wxTalkDone new_st
                                        else return []

                return $ (map $ (True,)) $ msgs <> msgs2
    where
        run_wx_monad :: forall a. WxTalkerMonad r m a -> ExceptT String m a
        run_wx_monad = flip runWxTalkerMonadE env
-- }}}1


-- 用于处理会话刚刚建立，产生一些输出消息
wxTalkerInputProcessJustInited :: forall m r s .
    (MonadIO m, MonadLogger m, Eq s
    , WxTalkerState r m s
    , WxTalkerDoneAction r m s
    ) =>
    (WxTalkerMonad r m (Maybe s))       -- ^ get state
    -> (s -> WxTalkerMonad r m ())      -- ^ set state
    -> r
    -> m (Either String WxppInMsgHandlerResult)
wxTalkerInputProcessJustInited get_st set_st env =
    runExceptT $ liftM (fromMaybe []) $ runMaybeT $ do
        (get_st', set_st') <- ioCachedGetSet get_st set_st
        st <- MaybeT $ run_wx_monad $ get_st'
        done <- lift $ run_wx_monad $ wxTalkIfDone st
        if done
            then do
                -- not in conversation
                $logWarnS wxppLogSource $ "inited state is done?"
                return []

            else do
                msgs <- lift $ ExceptT $ wxTalkerInputOneStep get_st' set_st' env Nothing
                msgs2 <- if null msgs
                            then do
                                $logErrorS wxppLogSource $
                                    "wxTalkerInputOneStep generate no output for newly created talk state."
                                return []
                            else do
                                lift $ liftM (map Just . fromMaybe []) $ runMaybeT $ do
                                    new_st <- MaybeT $ run_wx_monad $ get_st'
                                    done2 <- lift $ run_wx_monad $ wxTalkIfDone new_st
                                    if done2
                                        then lift $ run_wx_monad $ wxTalkDone new_st
                                        else return []

                return $ (map $ (True,)) $ msgs <> msgs2
    where
        run_wx_monad :: forall a. WxTalkerMonad r m a -> ExceptT String m a
        run_wx_monad = flip runWxTalkerMonadE env


-- | 这个小工具用于减少 getter, setter 访问数据库
-- 前提：状态的读写全部经过同一对包装过的值，是否在同一个线程不重要
--       例如若有完全另一个进程也作读写修改就有问题了
-- 使用这个函数是一种hack, 只是目前使用上看并未发发现问题
ioCachedGetSet :: (MonadIO m, MonadIO n) =>
    m (Maybe s)
    -> (s -> m ())
    -> n (m (Maybe s), s -> m ())
        -- ^ 新的 getter, setter
ioCachedGetSet get_f set_f = do
    ior <- liftIO $ newIORef Nothing
    let set_f' x = do
            set_f x
            liftIO $ writeIORef ior $ Just x

    let get_f' = (liftIO $ readIORef ior) >>= maybe get_f (return . Just)

    return (get_f', set_f')


newtype WrapTalkerState a = WrapTalkerState a
                            deriving (Eq)

instance (TalkerState a, Monad m) => WxTalkerState r m (WrapTalkerState a) where
    wxTalkPromptToInput _ (WrapTalkerState s) = mkWxTalkerMonad $ \_ -> runExceptT $ do
        let (m_reply, new_s) = talkPromptNext s
        return $ (maybe [] (return . WxppOutMsgText) m_reply, WrapTalkerState new_s)

    wxTalkHandleInput _ old_st@(WrapTalkerState s) ime = mkWxTalkerMonad $ \_ -> runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgText t -> handle_txt t

            WxppInMsgVoice _ _ (Just t) -> handle_txt t

            WxppInMsgVoice _ _ Nothing -> do
                let err = newErrorMessage (Message "unknown weixin message") (initialPos "")
                let (m_t, new_s) = talkNotUnderstanding s err
                return $ (maybe [] (return . Just . WxppOutMsgText) m_t, WrapTalkerState new_s)

            _ -> do
                -- 除了文字、语音之外，其它消息都认为不算是用户有意识的回答
                -- 我们这里不处理
                return ([], old_st)

            where
                handle_txt t = do
                    case parse (talkParser s) "" t of
                        Left err -> do
                            let (m_t, new_s) = talkNotUnderstanding s err
                            return $ (maybe [] (return . Just . WxppOutMsgText) m_t, WrapTalkerState new_s)

                        Right (m_t, new_s) -> do
                            return $ (maybe [] (return . Just . WxppOutMsgText) m_t, WrapTalkerState new_s)


    wxTalkIfDone (WrapTalkerState s) = mkWxTalkerMonad $ const $ return $ Right $ talkDone s


-- | as a place holder
data NullTalkerState = NullTalkerState
                        deriving (Eq, Ord, Enum, Bounded)

$(deriveJSON defaultOptions ''NullTalkerState)

instance Monad m => WxTalkerState r m NullTalkerState where
    wxTalkPromptToInput _ x   = mkWxTalkerMonad $ \_ -> runExceptT $ return $ ([], x)
    wxTalkHandleInput   _ x _ = mkWxTalkerMonad $ \_ -> runExceptT $ return $ ([], x)
    wxTalkIfDone        _   = mkWxTalkerMonad $ \_ -> runExceptT $ return $ True

instance Monad m => WxTalkerFreshState r m NullTalkerState where
    wxTalkInitiate _ = return $ Right NullTalkerState

instance Monad m => WxTalkerDoneAction r m NullTalkerState where
    wxTalkDone _ = mkWxTalkerMonad $ \_ -> return $ Right []


-- | 用于区分不同对话状态的字串
-- 实现时须人工保证不同对话状态使用不同的字串
class HasStateType a where
    getStateType :: Proxy a -> Text

getStateType' :: forall a. HasStateType a => a -> Text
getStateType' _ = getStateType (Proxy :: Proxy a)


instance HasStateType NullTalkerState where
    getStateType _ = "null"


-- | 用于个别有对环境有更多要求的会话
type family WxppTalkStateExtraEnv s :: Type


wxTalkGetAccessToken :: (MonadIO m, HasAccessTokenIO r, HasWxppAppID r)
                     => r
                     -> m (Either String AccessToken)
wxTalkGetAccessToken env = runExceptT $ do
    (liftIO $ wxppGetAccessTokenIO env)
        >>= maybe
                (throwError $ "no access token for app: " <> T.unpack (unWxppAppID app_id))
                (return . fst)
    where
        app_id = getWxppAppID env


-- vim: set foldmethod=marker:
