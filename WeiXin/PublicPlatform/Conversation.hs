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
import Text.Parsec.Prim

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
class Eq a => WxTalkerState m a where
    -- | 根据当前（刚更新过的）状态，提供一些响应
    wxTalkPromptNext :: a
                        -> m (Either String ([WxppOutMsg], a))
                        -- ^ 回应消息及新的状态

    -- | 对用户的消息作出响应
    wxTalkResponse :: a
                    -> WxppInMsgEntity
                    -> m (Either String (Maybe ([WxppOutMsg], a)))
                    -- ^ 在会话过程中，消息可能会被无条件传入这个函数处理
                    -- 因此，要表达这个函数是否真正有处理用户输入

    -- | 判断对话是否已结束
    wxTalkDone :: a -> m (Either String Bool)


wxTalkerRun :: (Monad m, WxTalkerState m a) =>
    Bool
    -> (m a)
    -> (a -> m ())
    -> Conduit WxppInMsgEntity (ExceptT String m) WxppOutMsg
wxTalkerRun skip_first_prompt get_state put_state =
    if skip_first_prompt
        then chk_wait_and_go
        else go
    where
        prompt = do
            st <- lift $ lift $ get_state
            (replies, new_st) <- lift $ ExceptT $ wxTalkPromptNext st
            mapM_ yield replies
            unless (st == new_st) $ do
                lift $ lift $ put_state new_st

        go = prompt >> chk_wait_and_go

        chk_wait_and_go = do
            done <- lift $ lift get_state >>= ExceptT . wxTalkDone
            if done
                then return ()
                else wait_and_go

        wait_and_go = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just t  -> do
                    st <- lift $ lift get_state
                    m_resp <- lift $ ExceptT $ wxTalkResponse st t
                    case m_resp of
                        Nothing -> do
                            wait_and_go

                        Just (replies, new_st) -> do
                            mapM_ yield replies
                            unless (st == new_st) $ do
                                lift $ lift $ put_state new_st
                            go


newtype WrapTalkerState a = WrapTalkerState a
                            deriving (Eq)

instance (Eq a, TalkerState a, Monad m) => WxTalkerState m (WrapTalkerState a) where
    wxTalkPromptNext (WrapTalkerState s) = runExceptT $ do
        let (m_reply, new_s) = talkPromptNext s
        return $ (maybe [] (return . WxppOutMsgText) m_reply, WrapTalkerState new_s)

    wxTalkResponse (WrapTalkerState s) ime = runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgText t -> do
                case parse (talkParser s) "" t of
                    Left err -> do
                        let (m_t, new_s) = talkNotUnderstanding s err
                        return $ Just $ (maybe [] (return . WxppOutMsgText) m_t, WrapTalkerState new_s)

                    Right (m_t, new_s) -> do
                        return $ Just $ (maybe [] (return . WxppOutMsgText) m_t, WrapTalkerState new_s)

            _ -> return Nothing

    wxTalkDone (WrapTalkerState s) = runExceptT $ return $ talkDone s
