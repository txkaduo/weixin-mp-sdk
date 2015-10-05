{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module WeiXin.PublicPlatform.Yesod.Site.Data where

import ClassyPrelude
import qualified Data.ByteString.Lazy       as LB
import Yesod
import Control.Monad.Logger
import Data.Aeson
import Data.Default

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.Yesod.Types
import WeiXin.PublicPlatform.Yesod.Model



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
                , wxppSubRunDBAction    :: WxppDbRunner -- ^ execute any DB actions
                , wxppSubSendOutMsgs    :: [(WxppOpenID, WxppOutMsg)] -> IO ()
                    -- ^ a computation to send outgoing messages
                , wxppSubMsgHandler     :: WxppInMsgHandler IO
                , wxppPreProcessInMsg   :: ( LB.ByteString
                                                -- ^ raw data of message (unparsed)
                                            -> Maybe WxppInMsgEntity
                                                -- ^ this is nothing only if caller cannot parse the message
                                            -> IO (Either String
                                                    (Maybe (LB.ByteString, Maybe WxppInMsgEntity))
                                                    )
                                            )
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
/p/oauth/return             OAuthReturnR        GET
/p/oauth/test               OAuthTestR          GET
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

