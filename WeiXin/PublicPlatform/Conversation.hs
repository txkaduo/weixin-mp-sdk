module WeiXin.PublicPlatform.Conversation
    ( module WeiXin.PublicPlatform.Conversation
    , module Text.Parsec.Text
    , module Text.Parsec.Error
    , module Text.Parsec.Prim
    ) where

import ClassyPrelude

import Control.Monad.State.Strict
import Data.Conduit

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
