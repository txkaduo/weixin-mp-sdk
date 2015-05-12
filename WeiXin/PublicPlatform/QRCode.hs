module WeiXin.PublicPlatform.QRCode where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types                     (Pair)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import Network.HTTP.Types                   (renderQueryText)
import qualified Blaze.ByteString.Builder   as BBB


-- | 创建永久场景二维码
wxppQrCodeCreatePersist ::
    (MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppScene
    -> m WxppMakeSceneResult
wxppQrCodeCreatePersist =
    wxppQrCodeCreateInternal
        [ "action_name" .= ("QR_LIMIT_SCENE" :: Text) ]


-- | 创建短期场景二维码
wxppQrCodeCreateTransient ::
    (MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Int      -- ^ TTL in seconds
    -> WxppIntSceneID
                -- ^ 文档说临时二维码只支持整数型的场景ID
    -> m WxppMakeSceneResult
wxppQrCodeCreateTransient atk ttl scene_int_id =
    wxppQrCodeCreateInternal
        [ "action_name" .= ("QR_SCENE" :: Text)
        , "expire_seconds" .= ttl
        ]
        atk
        (WxppSceneInt scene_int_id)

wxppQrCodeCreateInternal ::
    (MonadIO m, MonadLogger m, MonadThrow m) =>
    [Pair]
    -> AccessToken
    -> WxppScene
    -> m WxppMakeSceneResult
wxppQrCodeCreateInternal js_pairs (AccessToken { accessTokenData = atk }) scene = do
    let url = wxppRemoteApiBaseUrl ++ "/qrcode/create"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ toJSON $ object $
                ("scene" .= scene) : js_pairs)
        >>= asWxppWsResponseNormal'


-- | 可直接在网页上使用的显示二维码的URL
wxppQrCodeUrlByTicket :: QRTicket -> UrlText
wxppQrCodeUrlByTicket (QRTicket ticket) = UrlText $
    "https://mp.weixin.qq.com/cgi-bin/showqrcode"
        <> decodeUtf8 (BBB.toByteString $ renderQueryText True [ ("ticket", Just ticket ) ] )
