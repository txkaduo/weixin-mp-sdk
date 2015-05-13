{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module WeiXin.PublicPlatform.Yesod.Site.Data where

import ClassyPrelude
import Yesod
import Database.Persist.Quasi
import Control.Monad.Logger
import Network.Wai                          (Request)

import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.InMsgHandler


-- | 判断 WAI 请求是否来自可信的来源
-- 有若干 web 接口是打算暴露给同伴使用的
-- 这个函数负责检查这些请求是否可以执行
type RequestAuthChecker = WxppSub -> Request -> IO Bool

data WxppSub =
        WxppSub {
                wxppSubAppConfig        :: WxppAppConfig
                    -- ^ 所有配置信息
                , wxppSubAccessTokens   :: IO (Maybe AccessToken)
                    -- ^ a computation to get usable access token
                , wxppSubGetUnionID     :: AccessToken -> WxppOpenID -> IO (Maybe WxppUnionID)
                    -- ^ a function to get union_id by open_id
                , wxppSubSendOutMsgs    :: [WxppOutMsgEntity] -> IO ()
                    -- ^ a computation to send outgoing messages
                , wxppSubMsgHandler     :: WxppInMsgHandler (LoggingT IO)
                , wxppSubRunLoggingT    :: forall a. LoggingT IO a -> IO a
                , wxppSubTrustedWaiReq  :: RequestAuthChecker
                }

instance Show WxppSub where
    show (WxppSub app_config _ _ _ _ _ _) =
        "WxppSub: " ++ show app_config

-- | 为了支持多 app ，AppID 实际上是运行时才知道的
-- 所以对应的配置也是运行时才能查找出来，因为不一定能找到对应的配置
-- 结果就是个 Maybe。
newtype MaybeWxppSub = MaybeWxppSub { unMaybeWxppSub :: Maybe WxppSub }

mkYesodSubData "MaybeWxppSub" [parseRoutes|
/msg                        MessageR            GET POST
/x/atk                      GetAccessTokenR     GET
/x/union_id/#WxppOpenID     GetUnionIDR         GET
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

