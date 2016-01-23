{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Conversation.Misc where

import ClassyPrelude
import Data.Proxy
import qualified Data.Text                  as T
import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad.State                  (StateT(..))
import Data.List.NonEmpty                   as LNE hiding (insert)
import Data.Aeson                           (FromJSON(..), ToJSON(..))
import Text.Parsec                          (parse, eof, sourceLine, sourceColumn, errorPos, ParsecT)

import WeiXin.PublicPlatform.Conversation
import WeiXin.PublicPlatform.Conversation.TextParser
import WeiXin.PublicPlatform.Conversation.Message
import WeiXin.PublicPlatform.Class


data CommonTalkEnv = CommonTalkEnv
                        SomeWxppCacheClient
                        WxppAppID
                        (NonEmpty FilePath)     -- ^ base dir of message files

instance HasAccessToken CommonTalkEnv where
    wxppGetAccessToken (CommonTalkEnv cache app_id _ ) =
        wxppCacheGetAccessToken cache app_id

instance HasWxppAppID CommonTalkEnv where
    getWxppAppID (CommonTalkEnv _ app_id _ ) = app_id

instance HasSomeWxppCacheBackend CommonTalkEnv where
    getSomeWxppCacheBackend (CommonTalkEnv cache _ _) = cache

instance HasWxppOutMsgDir CommonTalkEnv where
    getWxppOutMsgDir (CommonTalkEnv _ _ base_dir ) = base_dir


-- | 取出待确认的内容
class HasConfirmContent r m a where
    getConfirmContent :: a -> WxTalkerMonad r m [WxppOutMsg]

-- | 当操作被取消时的提示语
class ToCancelledMessage r m a where
    toCancelledMessage :: a -> WxTalkerMonad r m [WxppOutMsg]

-- | 当用户消息不能被理解时的提示语
class ToDontUnderstandMessage r m a where
    toDontUnderstandMessage :: a -> WxTalkerMonad r m [WxppOutMsg]


generalInternalErrorMsg :: (LoadMsgMonad m, LoadMsgEnv r) =>
    r -> m (Either String WxppOutMsg)
generalInternalErrorMsg = $(loadTalkMessageTH "default-msgs" ("common" </> "internal_error"))

generalConfirmCancelledMsg :: (LoadMsgMonad m, LoadMsgEnv r) =>
    r -> m (Either String WxppOutMsg)
generalConfirmCancelledMsg = $(loadTalkMessageTH "default-msgs" ("common" </> "confirm" </> "cancelled"))

generalConfirmDontUnderstandMsg :: (LoadMsgMonad m, LoadMsgEnv r) =>
    r -> m (Either String WxppOutMsg)
generalConfirmDontUnderstandMsg = $(loadTalkMessageTH "default-msgs" ("common" </> "confirm" </> "dont_understand"))


-- | 这个对话状态用于包装另一个对话状态（待执行的操作）
-- 其对应的会话逻辑则是等待用户确认某种操作。
-- 如果用户同意，则执行其内的状态的 wxTalkDone 函数
data ConfirmState a = ConfirmState
                        a               -- ^ read-only state
                        (Maybe Bool)    -- ^ confirmed or not
                        deriving (Eq)

instance FromJSON a => FromJSON (ConfirmState a) where
    parseJSON = fmap (uncurry ConfirmState) . parseJSON

instance ToJSON a => ToJSON (ConfirmState a) where
    toJSON (ConfirmState x m_b) = toJSON (x, m_b)


instance (LoadMsgMonad m, LoadMsgEnv r) =>
    ToDontUnderstandMessage r m (ConfirmState a) where
    toDontUnderstandMessage _ = mkWxTalkerMonad $ \env -> do
                                    liftM (fmap return) $ generalConfirmDontUnderstandMsg env

instance HasStateType a => HasStateType (ConfirmState a) where
    getStateType _ = "ConfirmState" <> ":" <> getStateType (Proxy :: Proxy a)

instance
    ( LoadMsgMonad m, LoadMsgEnv r
    , Eq a
    , HasConfirmContent r m a
    , ToCancelledMessage r m a
    ) =>
    WxTalkerState r m (ConfirmState a)
    where
    wxTalkPromptToInput old_st@(ConfirmState rd_st m_confirmed) = do
        case m_confirmed of
            Nothing    -> liftM (, old_st) $ getConfirmContent rd_st

            Just False -> liftM (, old_st) $ toCancelledMessage rd_st

            Just True  -> return ([], old_st)


    wxTalkIfDone (ConfirmState _ m_confirmed) = return $ isJust m_confirmed


    wxTalkHandleInput old_st@(ConfirmState rd_st _m_confirmed) ime =
        mkWxTalkerMonad $ \env -> runExceptT $ do
            case wxppInMessage ime of
                WxppInMsgText t -> do
                    case parse generalParseConfirm "" t of
                        Left err -> do
                            $logWarn $ fromString $
                                "cannot parse user input as confirmation: " <> show err
                            liftM (, old_st) $
                                ExceptT $ flip runWxTalkerMonad env $ toDontUnderstandMessage old_st

                        Right b -> do
                            let new_st = ConfirmState rd_st (Just b)
                            return $ ([], new_st)

                _ -> do
                    liftM (, old_st) $
                        ExceptT $ flip runWxTalkerMonad env $ toDontUnderstandMessage old_st


instance ( LoadMsgMonad m
         , WxTalkerDoneAction r m a, ToCancelledMessage r m a) =>
    WxTalkerDoneAction r m (ConfirmState a)
    where
    wxTalkDone (ConfirmState rd_st m_confirmed) = do
        case m_confirmed of
            Just True -> wxTalkDone rd_st

            Just False -> toCancelledMessage rd_st

            _ -> do
                $logWarn $ "should never reach here: confirmation is Nothing"
                toCancelledMessage rd_st


-- | 使用指定的解释器解释文档输入
-- 若非文本，则返回 common/text_only.yml 的内容
-- 若文本不能被识别，则
parseInputOrLoadErrorMsg :: forall a m r.
    (LoadMsgEnv r, LoadMsgMonad m) =>
    (Text -> Text)      -- ^ function to apply before parsing
    -> r                -- ^ env
    -> FilePath         -- ^ error message
    -> ParsecT String () Identity a     -- ^ the text parser
    -> WxppInMsgEntity
    -> ExceptT String m (Either WxppOutMsg a)
parseInputOrLoadErrorMsg prep env err_msg_file p ime = do
    case wxppInMessage ime of
        WxppInMsgText t -> do
            let err_or_x = case runTextParser p' (prep t) of
                            Left (_err :: Text) -> runTextParser p' t
                            Right x -> Right x
            case err_or_x of
                Left err -> do
                    $logWarn $ T.unlines $
                                [ "cannot parse text: " <> t
                                , "error was: " <> err
                                ]
                    liftM Left $ ExceptT $ loadTalkMessage env err_msg_file

                Right x -> return $ Right x

        _ -> do
            liftM Left $ ExceptT $ loadTalkMessage env ("common" </> "text_only")

    where
        p' = p <* eof


parseInputThenOrLoadErrorMsg :: forall a m r s.
    (LoadMsgEnv r, LoadMsgMonad m) =>
    (Text -> Text)      -- ^ function to apply before parsing
    -> r                -- ^ env
    -> FilePath         -- ^ error message
    -> ParsecT String () Identity a     -- ^ the text parser
    -> WxppInMsgEntity
    -> s                -- ^ old state
    -> (a -> ExceptT String m ([WxppOutMsg], s))
    -> ExceptT String m ([WxppOutMsg], s)
parseInputThenOrLoadErrorMsg prep env err_msg_file p ime old_st f = do
    msg_or_x <- parseInputOrLoadErrorMsg prep env err_msg_file p ime
    case msg_or_x of
        Left msg -> return $ ([msg], old_st)
        Right x -> f x

parseInputThenOrLoadErrorMsgT :: forall a m r s.
    (LoadMsgEnv r, LoadMsgMonad m) =>
    (Text -> Text)      -- ^ function to apply before parsing
    -> FilePath         -- ^ error message
    -> ParsecT String () Identity a     -- ^ the text parser
    -> WxppInMsgEntity
    -> (a -> StateT s (ReaderT r (ExceptT String m)) [WxppOutMsg])
    -> StateT s (ReaderT r (ExceptT String m)) [WxppOutMsg]
parseInputThenOrLoadErrorMsgT prep err_msg_file p ime f = do
    env <- lift ask
    msg_or_x <- lift $ lift $ parseInputOrLoadErrorMsg prep env err_msg_file p ime
    case msg_or_x of
        Left msg -> return $ [msg]
        Right x -> f x


-- | 类似于 parseInputOrLoadErrorMsg，但加载消息的函数由调用者提供（而不仅仅是一个路径）
-- 输入的消息类型变成 Text，不用处理非文字信息的情况
parseInputOrLoadErrorMsg2 :: forall a m.
    (MonadLogger m, Monad m) =>
    (Text -> Text)      -- ^ function to apply before parsing
    -> m (Either String WxppOutMsg)     -- ^ function to load error message
    -> ParsecT String () Identity a     -- ^ the text parser
    -> Text
    -> ExceptT String m (Either WxppOutMsg a)
parseInputOrLoadErrorMsg2 prep load_err_msg_file p t = do
    let err_or_x = case runTextParser p' (prep t) of
                    Left (_err :: Text) -> runTextParser p' t
                    Right x -> Right x
    case err_or_x of
        Left err -> do
            $logWarn $ T.unlines $
                        [ "cannot parse text: " <> t
                        , "error was: " <> err
                        ]
            liftM Left $ ExceptT $ load_err_msg_file

        Right x -> return $ Right x

    where
        p' = p <* eof


-- | 类似于 parseInputThenOrLoadErrorMsg，但加载消息的函数由调用者提供（而不仅仅是一个路径）
-- 输入的消息类型变成 Text，不用处理非文字信息的情况
parseInputThenOrLoadErrorMsg2 :: forall a m s.
    (Monad m, MonadLogger m) =>
    (Text -> Text)      -- ^ function to apply before parsing
    -> m (Either String WxppOutMsg)     -- ^ function to load error message
    -> ParsecT String () Identity a     -- ^ the text parser
    -> Text
    -> s                -- ^ old state
    -> (a -> ExceptT String m ([WxppOutMsg], s))
    -> ExceptT String m ([WxppOutMsg], s)
parseInputThenOrLoadErrorMsg2 prep load_err_msg_file p t old_st f = do
    msg_or_x <- parseInputOrLoadErrorMsg2 prep load_err_msg_file p t
    case msg_or_x of
        Left msg -> return $ ([msg], old_st)
        Right x -> f x


-- | 类似于 parseInputThenOrLoadErrorMsgT，但加载消息的函数由调用者提供（而不仅仅是一个路径）
-- 输入的消息类型变成 Text，不用处理非文字信息的情况
parseInputThenOrLoadErrorMsgT2 :: forall a m r s.
    (Monad m, MonadLogger m) =>
    (Text -> Text)      -- ^ function to apply before parsing
    -> m (Either String WxppOutMsg)     -- ^ function to load error message
    -> ParsecT String () Identity a     -- ^ the text parser
    -> Text
    -> (a -> StateT s (ReaderT r (ExceptT String m)) [WxppOutMsg])
    -> StateT s (ReaderT r (ExceptT String m)) [WxppOutMsg]
parseInputThenOrLoadErrorMsgT2 prep load_err_msg_file p t f = do
    msg_or_x <- lift $ lift $ parseInputOrLoadErrorMsg2 prep load_err_msg_file p t
    case msg_or_x of
        Left msg -> return $ [msg]
        Right x -> f x


-- | 解释用户输入的文字
-- 如果输入的不是文字，则 throwError []
-- 如果输入的文字一点都不匹配，也 throwError []
-- 如果解释吃掉足够长的输入，则 throwError 出提示信息
parseUserInputText :: (MonadError [WxppOutMsg] m, MonadLogger m) =>
                    ParsecT String () Identity a
                        -- ^ 根据这个函数的行为，这个parser应吃掉尽可能多的输入
                    -> Text
                    -> WxppInMsgEntity
                    -> m a
parseUserInputText p help_txt ime = do
    parseUserInputX p (throwError []) (throwError $ [WxppOutMsgText help_txt]) ime

parseUserInputX :: (MonadLogger m) =>
                    ParsecT String () Identity a
                        -- ^ 根据这个函数的行为，这个parser应吃掉尽可能多的输入
                    -> m a      -- ^ call parser consumes nothing
                    -> m a      -- ^ call when parser consume something
                    -> WxppInMsgEntity
                    -> m a
parseUserInputX p nothing_done sth_done ime = do
    case wxppInMessage ime of
        WxppInMsgText t -> do
                            case parse p "" (T.unpack t) of
                                Left err -> do
                                    -- TODO: maybe using '<|>' to detect is better
                                    let (err_line, err_col) = sourceLine &&& sourceColumn $ errorPos err
                                    if err_col > 1 || err_line > 1
                                        then do
                                            -- 如果解释器已吃掉了一部分输入，说明命令有可能匹配，只是有点错
                                            $logError $ fromString $ "cannot parse user input: " <> show err
                                            sth_done
                                        else
                                            nothing_done

                                Right x -> return x

        _               -> nothing_done
