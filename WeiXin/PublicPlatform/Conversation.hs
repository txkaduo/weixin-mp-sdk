module WeiXin.PublicPlatform.Conversation
    ( module WeiXin.PublicPlatform.Conversation
    , module Text.Parsec.Text
    , module Text.Parsec.Error
    , module Text.Parsec.Prim
    ) where

import ClassyPrelude

import Control.Monad.State.Strict hiding (mapM_)
import Control.Monad.Trans.Except
import Data.Conduit
import qualified Data.Conduit.List          as CL

import Text.Parsec.Text
import Text.Parsec.Error
import Text.Parsec.Pos                      (initialPos)
import Text.Parsec.Prim
import Data.Aeson.TH                        (deriveJSON, defaultOptions)

import WeiXin.PublicPlatform.Types

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
    -> Conduit Text m Text
talkerRun = talkerRun' False

talkerRun' :: (Monad m, TalkerState a) =>
    Bool
    -> (m a)
    -> (a -> m ())
    -> Conduit Text m Text
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
    Conduit Text (StateT a m) Text
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
    send_intput =$= cond $$ CL.consume


-- | 支持更多操作的接口: 例如可以使用 IO 之类
-- 处理的用户输入也不仅仅是文字输入
class WxTalkerState m a where
    -- | 为用户下一个输入提供提示
    -- 注意：这个方法会在 wxTalkHandleInput 之后调用
    wxTalkPromptToInput :: a
                        -> m (Either String ([WxppOutMsg], a))
                        -- ^ 回应消息及新的状态

    -- | 处理用户的输入，并产生结果
    -- 在会话过程中，消息可能会被无条件传入这个函数处理
    -- 要表达这个函数是否真正有处理用户输入，因此结果是个 Maybe
    wxTalkHandleInput :: a
                    -> WxppInMsgEntity
                    -> m (Either String ([WxppOutMsg], a))
                    -- ^ 在会话过程中，消息可能会被无条件传入这个函数处理

    -- | 判断对话是否已结束
    wxTalkIfDone :: a -> m (Either String Bool)


class WxTalkerDoneAction m a where
    wxTalkDone :: a -> m (Either String [WxppOutMsg])


wxTalkerRun :: (Monad m, Eq a, WxTalkerState m a) =>
    Bool
    -> (m (Either String a))
    -> (a -> m (Either String ()))
    -> Conduit WxppInMsgEntity (ExceptT String m) WxppOutMsg
wxTalkerRun skip_first_prompt get_state put_state =
    if skip_first_prompt
        then chk_wait_and_go
        else go
    where
        prompt = do
            st <- lift $ ExceptT $ get_state
            (replies, new_st) <- lift $ ExceptT $ wxTalkPromptToInput st
            mapM_ yield replies
            unless (st == new_st) $ do
                lift $ ExceptT $ put_state new_st

        go = prompt >> chk_wait_and_go

        chk_wait_and_go = do
            done <- lift $ ExceptT get_state >>= ExceptT . wxTalkIfDone
            if done
                then return ()
                else wait_and_go

        wait_and_go = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just t  -> do
                    st <- lift $ ExceptT get_state
                    (replies, new_st) <- lift $ ExceptT $ wxTalkHandleInput st t
                    mapM_ yield replies
                    unless (st == new_st) $ do
                        lift $ ExceptT $ put_state new_st
                    go


wxTalkerInputOneStep :: (Monad m, WxTalkerState m s, Eq s) =>
    m (Either String s)                 -- ^ get state
    -> (s -> m (Either String ()))      -- ^ set state
    -> Maybe WxppInMsgEntity
        -- ^ 新建立的会话时，用户输入理解为 Nothing
        -- 之后的调用都应该是 Just
    -> m (Either String [WxppOutMsg])
        -- ^ 状态机的输出文字
wxTalkerInputOneStep get_st set_st m_input = runExceptT $ do
    send_intput =$= cond $$ CL.consume
    where
        send_intput = case m_input of
                        Nothing -> return ()
                        Just x -> yield x
        cond = wxTalkerRun (isJust m_input) get_st set_st


newtype WrapTalkerState a = WrapTalkerState a
                            deriving (Eq)

instance (Eq a, TalkerState a, Monad m) => WxTalkerState m (WrapTalkerState a) where
    wxTalkPromptToInput (WrapTalkerState s) = runExceptT $ do
        let (m_reply, new_s) = talkPromptNext s
        return $ (maybe [] (return . WxppOutMsgText) m_reply, WrapTalkerState new_s)

    wxTalkHandleInput (WrapTalkerState s) ime = runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgText t -> do
                case parse (talkParser s) "" t of
                    Left err -> do
                        let (m_t, new_s) = talkNotUnderstanding s err
                        return $ (maybe [] (return . WxppOutMsgText) m_t, WrapTalkerState new_s)

                    Right (m_t, new_s) -> do
                        return $ (maybe [] (return . WxppOutMsgText) m_t, WrapTalkerState new_s)

            _ -> do
                let err = newErrorMessage (Message "unknown weixin message") (initialPos "")
                let (m_t, new_s) = talkNotUnderstanding s err
                return $ (maybe [] (return . WxppOutMsgText) m_t, WrapTalkerState new_s)

    wxTalkIfDone (WrapTalkerState s) = runExceptT $ return $ talkDone s


-- | as a place holder
data NullTalkerState = NullTalkerState
                        deriving (Eq, Ord, Enum, Bounded)

$(deriveJSON defaultOptions ''NullTalkerState)

instance Monad m => WxTalkerState m NullTalkerState where
    wxTalkPromptToInput x   = return $ Right ([], x)
    wxTalkHandleInput   x _ = return $ Right ([], x)
    wxTalkIfDone        _   = return $ Right True

instance Monad m => WxTalkerDoneAction m NullTalkerState where
    wxTalkDone _ = return $ Right []


data SomeWxTalkerState m = forall a. (WxTalkerState m a, WxTalkerDoneAction m a) =>
                                SomeWxTalkerState a

instance Monad m => WxTalkerState m (SomeWxTalkerState m) where
    wxTalkPromptToInput (SomeWxTalkerState x)    = liftM (fmap $ second SomeWxTalkerState) $
                                                    wxTalkPromptToInput x

    wxTalkHandleInput (SomeWxTalkerState x)   = liftM (fmap $ second SomeWxTalkerState) .
                                                    wxTalkHandleInput x

    wxTalkIfDone (SomeWxTalkerState x)      = wxTalkIfDone x


instance WxTalkerDoneAction m (SomeWxTalkerState m) where
    wxTalkDone (SomeWxTalkerState x) = wxTalkDone x
