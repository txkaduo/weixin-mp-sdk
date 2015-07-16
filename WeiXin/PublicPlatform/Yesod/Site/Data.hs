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
import Data.Bits                            ((.&.))
import Network.Socket                       (SockAddr(..))
import Network.Wai                          (remoteHost)
import Data.Aeson
import Data.Default

import Database.Persist.Sql

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.InMsgHandler


wxppSubModelsDef ::
#if MIN_VERSION_persistent(2, 0, 0)
    [EntityDef]
#else
    [EntityDef SqlType]
#endif
wxppSubModelsDef = $(persistFileWith lowerCaseSettings "models")

share [mkPersist sqlSettings, mkMigrate "migrateAllWxppSubModels"]
                    $(persistFileWith lowerCaseSettings "models")


-- | 判断 WAI 请求是否来自可信的来源
-- 有若干 web 接口是打算暴露给同伴使用的
-- 这个函数负责检查这些请求是否可以执行
type RequestAuthChecker = Request -> IO Bool

alwaysDenyRequestAuthChecker :: RequestAuthChecker
alwaysDenyRequestAuthChecker _ = return False

-- | 总是通过检查
-- 使用这个函数意味着系统有其它手段作安全限制
alwaysAllowRequestAuthChecker :: RequestAuthChecker
alwaysAllowRequestAuthChecker _ = return True

loopbackOnlyRequestAuthChecker :: RequestAuthChecker
loopbackOnlyRequestAuthChecker req = return $ isLoopbackSockAddr $ remoteHost req

isLoopbackSockAddr :: SockAddr -> Bool
isLoopbackSockAddr addr =
    case addr of
        SockAddrInet _ w        -> w .&. 0xFF  == 127
        SockAddrInet6 _ _ w _   -> w == (0, 0, 0, 1)
        _                       -> False


data WxppSubsiteOpts = WxppSubsiteOpts {
                            wxppSubTrustedWaiReq    :: RequestAuthChecker
                            , wxppSubFakeQRTicket   :: Bool
                            , wxppSubMakeupUnionID  :: Bool
                            }

instance Default WxppSubsiteOpts where
    def = WxppSubsiteOpts alwaysDenyRequestAuthChecker False False

instance FromJSON WxppSubsiteOpts where
    parseJSON = withObject "WxppSubsiteOpts" $ \obj -> do
                    WxppSubsiteOpts
                        <$> parse_auth_mode obj
                        <*> ( obj .:? "fake-qrcode" .!= wxppSubFakeQRTicket def)
                        <*> ( obj .:? "fake-union-id" .!= wxppSubMakeupUnionID def)
                where
                    parse_auth_mode obj = do
                        mode <- obj .:? "api-auth-mode" .!= "always-deny"
                        case mode of
                             "always-deny" -> pure alwaysDenyRequestAuthChecker
                             "always-allow" -> pure alwaysAllowRequestAuthChecker
                             "loopback-only" -> pure loopbackOnlyRequestAuthChecker
                             _ -> fail $ "unknown auth-mode: " ++ mode


-- | 为每个运行的 App 对应一个 subsite
data WxppSub =
        WxppSub {
                wxppSubAppConfig        :: WxppAppConfig
                    -- ^ 所有配置信息
                , wxppSubCacheBackend   :: SomeWxppCacheBackend
                , wxppSubRunDBAction    ::
                                        -- XXX: 这里写死两个事实
                                        -- * persistent 版本要 2.0
                                        -- * 只能是 SQL 类型数据库
                                        forall a m. (MonadIO m, MonadBaseControl IO m) =>
                                                        SqlPersistT m a -> m a
                    -- ^ execute any DB actions
                , wxppSubSendOutMsgs    :: [WxppOutMsgEntity] -> IO ()
                    -- ^ a computation to send outgoing messages
                , wxppSubMsgHandler     :: WxppInMsgHandler (LoggingT IO)
                , wxppSubMsgMiddlewares :: [SomeWxppInMsgProcMiddleware (LoggingT IO)]
                , wxppSubRunLoggingT    :: forall a m. LoggingT m a -> m a
                , wxppSubOptions        :: WxppSubsiteOpts
                }

instance Show WxppSub where
    show x = "WxppSub: " ++ show (wxppSubAppConfig x)

-- | 为了支持多 app ，AppID 实际上是运行时才知道的
-- 所以对应的配置也是运行时才能查找出来，因为不一定能找到对应的配置
-- 结果就是个 Maybe。
newtype MaybeWxppSub = MaybeWxppSub { unMaybeWxppSub :: IO (Maybe WxppSub) }

mkYesodSubData "MaybeWxppSub" [parseRoutes|
/msg                        MessageR            GET POST
-- 修改以下的路径，记得修改 WeiXin.PublicPlatform.Center 里的相应路径
/x/atk                      GetAccessTokenR     GET
/x/union_id/#WxppOpenID     GetUnionIDR         GET
/x/user/info/#WxppOpenID    QueryUserInfoR      GET
/x/qrcode/persist           CreateQrCodePersistR POST
/x/qrcode/sm                ShowSimulatedQRCodeR GET
/init/cached-user-info      InitCachedUsersR    GET
|]


-- | 为 App 无关的接口打包成一个 subsite
data WxppSubNoApp = WxppSubNoApp {
                        wxppSubNoAppUnionIdByOpenId     :: WxppUnionID -> IO [(WxppOpenID, WxppAppID)]
                        , wxppSubNoAppRunLoggingT       :: forall a m. LoggingT m a -> m a
                        , wxppSubNoAppCheckWaiReq       :: RequestAuthChecker
                    }

mkYesodSubData "WxppSubNoApp" [parseRoutes|
/union-to-open/#WxppUnionID     LookupOpenIDByUnionIDR      GET
|]

-- | 把一些数据打包成字串后，作为模拟的 ticket
type FakeQRTicket = (WxppScene, UrlText)

