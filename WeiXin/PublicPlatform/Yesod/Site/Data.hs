{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module WeiXin.PublicPlatform.Yesod.Site.Data where

import ClassyPrelude
import Yesod
import Database.Persist.Quasi
import Control.Monad.Logger

import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.InMsgHandler


data WxppSub =
        WxppSub {
                wxppSubAppConfig        :: WxppAppConfig
                    -- ^ 所有配置信息
                , wxppSubAccessTokens   :: IO (Maybe AccessToken)
                    -- ^ a computation to get usable access token
                , wxppSubSendOutMsgs    :: [WxppOutMsgEntity] -> IO ()
                    -- ^ a computation to send outgoing messages
                , wxppSubDataDir        :: FilePath
                , wxppSubMsgHandler     :: WxppInMsgHandler (LoggingT IO)
                , wxppSubRunLoggingT    :: forall a. LoggingT IO a -> IO a
                }

instance Show WxppSub where
    show (WxppSub app_config _ _ _ _ _) =
        "WxppSub: " ++ show app_config

mkYesodSubData "WxppSub" [parseRoutes|
/msg            MessageR        GET POST
/menu/reload    ReloadMenuR     GET
/menu/query     QueryMenuR      GET
|]

wxppSubModelsDef ::
#if MIN_VERSION_persistent(2, 0, 0)
    [EntityDef]
#else
    [EntityDef SqlType]
#endif
wxppSubModelsDef = $(persistFileWith lowerCaseSettings "models")

share [mkPersist sqlSettings, mkMigrate "migrateAllWxppSubModels"]
                    $(persistFileWith lowerCaseSettings "models")

