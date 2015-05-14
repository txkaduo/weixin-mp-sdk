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
import Data.Default

import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.InMsgHandler


-- | 判断 WAI 请求是否来自可信的来源
-- 有若干 web 接口是打算暴露给同伴使用的
-- 这个函数负责检查这些请求是否可以执行
type RequestAuthChecker = WxppSub -> Request -> IO Bool

data WxppSubsiteOpts = WxppSubsiteOpts {
                            wxppSubTrustedWaiReq    :: RequestAuthChecker
                            , wxppSubFakeQRTicket   :: Bool
                            , wxppSubMakeupUnionID  :: Bool
                            }

alwaysDenyRequestAuthChecker :: RequestAuthChecker
alwaysDenyRequestAuthChecker _ _ = return False

instance Default WxppSubsiteOpts where
    def = WxppSubsiteOpts alwaysDenyRequestAuthChecker False False

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
                , wxppSubOptions        :: WxppSubsiteOpts
                }

instance Show WxppSub where
    show x = "WxppSub: " ++ show (wxppSubAppConfig x)

-- | 为了支持多 app ，AppID 实际上是运行时才知道的
-- 所以对应的配置也是运行时才能查找出来，因为不一定能找到对应的配置
-- 结果就是个 Maybe。
newtype MaybeWxppSub = MaybeWxppSub { unMaybeWxppSub :: Maybe WxppSub }

mkYesodSubData "MaybeWxppSub" [parseRoutes|
/msg                        MessageR            GET POST
-- 修改以下的路径，记得修改 WeiXin.PublicPlatform.Center 里的相应路径
/x/atk                      GetAccessTokenR     GET
/x/union_id/#WxppOpenID     GetUnionIDR         GET
/x/user/info/#WxppOpenID    QueryUserInfoR      GET
/x/qrcode/persist           CreateQrCodePersistR POST
/x/qrcode/sm                ShowSimulatedQRCodeR GET
|]

-- | 把一些数据打包成字串后，作为模拟的 ticket
type FakeQRTicket = (WxppScene, UrlText)

wxppSubModelsDef ::
#if MIN_VERSION_persistent(2, 0, 0)
    [EntityDef]
#else
    [EntityDef SqlType]
#endif
wxppSubModelsDef = $(persistFileWith lowerCaseSettings "models")

share [mkPersist sqlSettings, mkMigrate "migrateAllWxppSubModels"]
                    $(persistFileWith lowerCaseSettings "models")

