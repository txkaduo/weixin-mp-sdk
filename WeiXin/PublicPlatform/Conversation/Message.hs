module WeiXin.PublicPlatform.Conversation.Message where

-- {{{1 imports
import ClassyPrelude
import qualified Control.Exception.Safe as ExcSafe
import Language.Haskell.TH
import Control.Monad.Logger
import Control.Monad.Except
import Data.List.NonEmpty                   as LNE hiding (insert)

import WeiXin.PublicPlatform.Conversation
import WeiXin.PublicPlatform.Utils
import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Media
-- }}}1


type LoadMsgMonad m = (MonadIO m, MonadLoggerIO m, ExcSafe.MonadCatch m)

type LoadMsgEnv r = ( HasWxppOutMsgDir r
                    , HasSomeWxppCacheBackend r
                    , HasAccessTokenIO r
                    , HasWxppAppID r
                    , HasWreqSession r
                    , HasWxppUrlConfig r
                    )


talkerMessageDir :: HasWxppOutMsgDir a => a -> NonEmpty FilePath
talkerMessageDir env = fmap (</> "talk") $ getWxppOutMsgDir env


loadTalkMessage :: ( LoadMsgMonad m, LoadMsgEnv r) =>
    r -> FilePath -> m (Either String WxppOutMsg)
loadTalkMessage env sub_path = runExceptT $ do
    msg_l <- ExceptT $ runDelayedYamlLoaderL
                (talkerMessageDir env)
                (mkDelayedYamlLoader $ setExtIfNotExist "yml" $ sub_path)

    let api_env = uncurry WxppApiEnv $ (getWreqSession &&& getWxppUrlConfig) env

    flip runReaderT api_env $ do
      fromWxppOutMsgL
        (getWxppOutMsgDir env)
        (getSomeWxppCacheBackend env)
        (lift $ ExceptT $ wxTalkGetAccessToken env)
        msg_l

loadTalkMessage' :: ( LoadMsgMonad m, LoadMsgEnv r) =>
                    FilePath
                    -> WxTalkerMonad r m WxppOutMsg
loadTalkMessage' sub_path = mkWxTalkerMonad $ \env -> loadTalkMessage env sub_path


loadTalkMsgHelper :: FilePath -> FilePath -> IO WxppOutMsg
loadTalkMsgHelper base_path sub_path = runDelayedYamlLoaderExc base_path $
                                mkDelayedYamlLoader $ setExtIfNotExist "yml" $ sub_path

-- | May throw IOError
loadTalkMessage_IOE :: ( LoadMsgMonad m, LoadMsgEnv r) =>
    r -> FilePath -> m (Either String WxppOutMsg)
loadTalkMessage_IOE env sub_path = runExceptT $ do
    msg_l <- withExceptT err_to_str $ ExceptT $ runDelayedYamlLoaderL_IOE
                (talkerMessageDir env)
                (mkDelayedYamlLoader $ setExtIfNotExist "yml" $ sub_path)

    let api_env = uncurry WxppApiEnv $ (getWreqSession &&& getWxppUrlConfig) env

    flip runReaderT api_env $ do
      fromWxppOutMsgL
        (getWxppOutMsgDir env)
        (getSomeWxppCacheBackend env)
        (lift $ ExceptT $ wxTalkGetAccessToken env)
        msg_l
    where
        err_to_str err = "failed to load '" <> sub_path <> "': " <> show err

loadTalkMessage_IOE' :: ( LoadMsgMonad m, LoadMsgEnv r) =>
                    FilePath
                    -> WxTalkerMonad r m WxppOutMsg
loadTalkMessage_IOE' sub_path = mkWxTalkerMonad $ \env -> loadTalkMessage_IOE env sub_path

loadTalkMessageTH :: FilePath -> FilePath -> Q Exp
loadTalkMessageTH default_msgs_dir sub_path = do
                        fallback_v <- runIO $ loadTalkMsgHelper default_msgs_dir sub_path
                        [| \env -> loadTalkMessage_IOE env sub_path
                                        `ExcSafe.catch`
                                            \ err -> do
                                                unless (isDoesNotExistError err) $ do
                                                    $logErrorS wxppLogSource $ fromString $
                                                        "fallback to default message because of failing to load message file '"
                                                        <> sub_path <> "', error was: " <> show err
                                                return $ Right fallback_v
                                            |]

loadTalkMessageTH' :: FilePath -> FilePath -> Q Exp
loadTalkMessageTH' default_msgs_dir sub_path = do
                        fallback_v <- runIO $ loadTalkMsgHelper default_msgs_dir sub_path
                        [| \env -> loadTalkMessage_IOE' env sub_path
                                        `ExcSafe.catch`
                                            \ err -> do
                                                unless (isDoesNotExistError err) $ do
                                                    $logErrorS wxppLogSource $ fromString $
                                                        "fallback to default message because of failing to load message file '"
                                                        <> sub_path <> "', error was: " <> show err
                                                return $ fallback_v
                                            |]
