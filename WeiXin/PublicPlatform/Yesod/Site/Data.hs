{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module WeiXin.PublicPlatform.Yesod.Site.Data where

-- {{{1 imports
import ClassyPrelude
#if MIN_VERSION_base(4, 13, 0)
import Control.Monad (MonadFail(..))
#else
#endif
import qualified Data.ByteString.Lazy as LB
import Yesod
import Control.Monad.Logger
import Data.Aeson
import Data.Default
import Data.List.NonEmpty (NonEmpty)

import Yesod.Helpers.Logger (LoggingTRunner(..))

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.ThirdParty
import WeiXin.PublicPlatform.Yesod.Types
import WeiXin.PublicPlatform.Yesod.Model

import Yesod.Compat
-- }}}1



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



data WxppMsgProcessor = WxppMsgProcessor
  { wxppSendOutMsgs     :: WxppAppID -> [(WxppOpenID, WxppOutMsg)] -> IO ()
                         -- ^ a computation to send outgoing messages
                         -- 1st: my app id.
                         --      若是第三方平台

  , wxppMsgHandler      :: WxppInMsgHandler IO

  , wxppPreProcessInMsg :: ProcAppIdInfo
                        -> LB.ByteString
                         -- raw data of message (unparsed)
                        -> WxppInMsgEntity
                        -- this is nothing only if caller cannot parse the message
                        -> IO (Either String
                                (Maybe (LB.ByteString, WxppInMsgEntity))
                                )

  , wxppPostProcessInMsg :: ProcAppIdInfo
                         -> LB.ByteString
                         -- raw data of message (unparsed)
                         -> WxppInMsgEntity
                         -- this is nothing only if caller cannot parse the message
                         -> WxppInMsgHandlerResult
                         -> IO (Either String WxppInMsgHandlerResult)

  , wxppOnProcessInMsgError :: ProcAppIdInfo
                            -> LB.ByteString
                            -- raw data of message (unparsed)
                            -> WxppInMsgEntity
                            -- this is nothing only if caller cannot parse the message

                            -> String
                            -> IO (Either String ())

  , wxppOnParseInMsgError :: ProcAppIdInfo -> LB.ByteString -> IO ()
  -- ^ called when incoming message cannot be parsed
  }


class HasWxppProcessor a where
  getWxppProcessor :: a -> WxppMsgProcessor

-- | 为每个运行的 App 对应一个 subsite
data WxppSub =
        WxppSub { wxppSubAppConf        :: WxppAppConf
                , wxppSubCacheBackend   :: SomeWxppCacheClient
                , wxppSubRunDBAction    :: WxppDbRunner -- ^ execute any DB actions
                , wxppSubProcessor      :: WxppMsgProcessor
                , wxppSubRunLoggingT    :: forall a m. LoggingT m a -> m a
                , wxppSubOptions        :: WxppSubsiteOpts
                , wxppSubApiEnv         :: WxppApiEnv
                }

instance Show WxppSub where
    show x = "WxppSub: " ++ show (getWxppAppID x)

instance HasWxppToken WxppSub where
  getWxppToken = getWxppToken . wxppSubAppConf

instance LoggingTRunner WxppSub where
  runLoggingTWith = wxppSubRunLoggingT

instance HasWxppAppID WxppSub where
  getWxppAppID = getWxppAppID . wxppSubAppConf

instance HasAesKeys WxppSub where
  getAesKeys = getAesKeys . wxppSubAppConf

instance HasWxppSecret WxppSub where
  getWxppSecret = getWxppSecret . wxppSubAppConf

instance HasWxppProcessor WxppSub where
  getWxppProcessor = wxppSubProcessor


-- | 为了支持多 app ，AppID 实际上是运行时才知道的
-- 所以对应的配置也是运行时才能查找出来，因为不一定能找到对应的配置
-- 结果就是个 Maybe。
newtype MaybeWxppSub = MaybeWxppSub { unMaybeWxppSub :: IO (Maybe WxppSub) }

instance RenderMessage MaybeWxppSub FormMessage where
  renderMessage _ _ = defaultFormMessage


mkYesodSubData "MaybeWxppSub" [parseRoutes|
/msg                        MessageR            GET POST

/p/oauth/callback           OAuthCallbackR      GET
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


-- | 第三方平台所有端点
data WxppTpSub = WxppTpSub
  { wxppTpSubComponentAppId     :: WxppAppID
  , wxppTpSubToken              :: Token
  , wxppTpSubAesKeys            :: NonEmpty AesKey
  -- , wxppTpSubCacheBackend       :: SomeWxppCacheClient
  , wxppTpSubRunLoggingT        :: forall a m. LoggingT m a -> m a
  , wxppTpSubProcessor          :: WxppMsgProcessor
  -- ^ 处理公众号消息事件的逻辑
  , wxppTpSubHandlerEventNotice :: forall master. Yesod master
                                => WxppTpEventNotice
                                -> SubHandlerOf WxppTpSub master (Either String Text)

  -- ^ 处理第三方平台事件通知的逻辑
  }

instance Show WxppTpSub where
    show x = "WxppTpSub: " ++ show (wxppTpSubComponentAppId x)

instance HasWxppToken WxppTpSub where
  getWxppToken = wxppTpSubToken

instance LoggingTRunner WxppTpSub where
  runLoggingTWith = wxppTpSubRunLoggingT

instance RenderMessage WxppTpSub FormMessage where
  renderMessage _ _ = defaultFormMessage

instance HasAesKeys WxppTpSub where
  getAesKeys = toList . wxppTpSubAesKeys

instance HasWxppAppID WxppTpSub where
  getWxppAppID = wxppTpSubComponentAppId

instance HasWxppProcessor WxppTpSub where
  getWxppProcessor = wxppTpSubProcessor


mkYesodSubData "WxppTpSub" [parseRoutes|
-- 第三方平台事件接收
/notice                   TpEventNoticeR      GET POST
/auther/#WxppAppID/msg    TpMessageR          GET POST
|]


-- | 把一些数据打包成字串后，作为模拟的 ticket
type FakeQRTicket = (WxppScene, UrlText)

