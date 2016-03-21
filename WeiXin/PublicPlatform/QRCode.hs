module WeiXin.PublicPlatform.QRCode where

import ClassyPrelude
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Control.Monad.Reader                 (asks)
import Data.Aeson
import Data.Aeson.Types                     (Pair)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import Network.HTTP.Types                   (renderQueryText)
import qualified Blaze.ByteString.Builder   as BBB


-- | 创建永久场景二维码
wxppQrCodeCreatePersist :: (WxppApiMonad env m)
                        => AccessToken
                        -> WxppScene
                        -> m WxppMakeSceneResult
wxppQrCodeCreatePersist atk scene = do
    let action = case scene of
                    WxppSceneInt {} -> "QR_LIMIT_SCENE" :: Text
                    WxppSceneStr {} -> "QR_LIMIT_STR_SCENE"
    wxppQrCodeCreateInternal
        [ "action_name" .= action ]
        atk
        scene


-- | 创建短期场景二维码
wxppQrCodeCreateTransient :: (WxppApiMonad env m)
                          => AccessToken
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

wxppQrCodeCreateInternal :: (WxppApiMonad env m)
                         => [Pair]
                         -> AccessToken
                         -> WxppScene
                         -> m WxppMakeSceneResult
wxppQrCodeCreateInternal js_pairs (AccessToken { accessTokenData = atk }) scene = do
    let url = wxppRemoteApiBaseUrl ++ "/qrcode/create"
        opts = defaults & param "access_token" .~ [ atk ]

    sess <- asks getWreqSession
    liftIO (WS.postWith opts sess url $ toJSON $ object $
                ( "action_info" .= object [ "scene" .= scene ] ) : js_pairs)
        >>= asWxppWsResponseNormal'


-- | 可直接在网页上使用的显示二维码的URL
wxppQrCodeUrlByTicket :: QRTicket -> UrlText
wxppQrCodeUrlByTicket (QRTicket ticket) = UrlText $
    "https://mp.weixin.qq.com/cgi-bin/showqrcode"
        <> decodeUtf8 (BBB.toByteString $ renderQueryText True [ ("ticket", Just ticket ) ] )
