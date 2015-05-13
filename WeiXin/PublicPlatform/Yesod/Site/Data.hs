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
                , wxppSubMsgHandler     :: WxppInMsgHandler (LoggingT IO)
                , wxppSubRunLoggingT    :: forall a. LoggingT IO a -> IO a
                }

instance Show WxppSub where
    show (WxppSub app_config _ _ _ _) =
        "WxppSub: " ++ show app_config

-- | 为了支持多 app ，AppID 实际上是运行时才知道的
-- 所以对应的配置也是运行时才能查找出来，因为不一定能找到对应的配置
-- 结果就是个 Maybe。
newtype MaybeWxppSub = MaybeWxppSub { unMaybeWxppSub :: Maybe WxppSub }

mkYesodSubData "MaybeWxppSub" [parseRoutes|
/msg            MessageR        GET POST
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

