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
    talkPromptNext :: a -> (Maybe Text, Maybe a)

    talkNotUnderstanding :: a -> ParseError -> (Maybe Text, Maybe a)

    -- | 解释出 Nothing 代表对话结束
    talkParser :: a -> Parser (Maybe Text, Maybe a)

    talkDone :: a -> Bool


talkerRun :: (Monad m, TalkerState a) =>
    (m (Maybe a))
    -> (Maybe a -> m ())
    -> Conduit Text m Text
talkerRun get_state put_state = go
    where
        prompt = do
            m_st <- lift get_state
            case m_st of
                Nothing -> return ()
                Just st -> do
                    let (m_t, ms) = talkPromptNext st
                    maybe (return ()) yield m_t
                    lift $ put_state ms

        go = do
            prompt
            done <- liftM (maybe True talkDone) $ lift get_state
            if done
                then return ()
                else wait_and_go

        wait_and_go = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just t  -> do
                    m_st <- lift get_state
                    case m_st of
                        Nothing -> return ()
                        Just st -> do
                            case parse (talkParser st) "" t of
                                Left err -> do
                                    let (m_t, ms) = talkNotUnderstanding st err
                                    maybe (return ()) yield m_t
                                    lift $ put_state ms

                                Right (m_reply, m_new_st) -> do
                                    maybe (return ()) yield m_reply
                                    lift $ put_state m_new_st
                            go


talkerTestRunner :: (Monad m, TalkerState a) =>
    Conduit Text (StateT (Maybe a) m) Text
talkerTestRunner = talkerRun get put
