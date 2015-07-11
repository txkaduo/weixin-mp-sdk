module WeiXin.PublicPlatform.Conversation
    ( module WeiXin.PublicPlatform.Conversation
    , module Text.Parsec.Text
    , module Text.Parsec.Error
    , module Text.Parsec.Prim
    ) where

import ClassyPrelude

import Control.Monad.State.Strict
import Data.Conduit
import qualified Data.Conduit.List          as CL

import Text.Parsec.Text
import Text.Parsec.Error
import Text.Parsec.Prim


class TalkerState a where
    talkPromptNext :: a -> (Maybe Text, a)

    talkNotUnderstanding :: a -> ParseError -> (Maybe Text, a)

    -- | 解释出 Nothing 代表对话结束
    talkParser :: a -> Parser (Maybe Text, a)

    talkDone :: a -> Bool


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
