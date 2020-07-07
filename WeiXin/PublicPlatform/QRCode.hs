module WeiXin.PublicPlatform.QRCode where

-- {{{1 imports
import ClassyPrelude
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Reader                 (asks)
#endif
import Data.Aeson
import Data.Aeson.Types                     (Pair)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import Network.HTTP.Types                   (renderQueryText)
import qualified Blaze.ByteString.Builder   as BBB
-- }}}1


-- | 创建永久场景二维码
wxppQrCodeCreatePersist :: (WxppApiMonad env m)
                        => AccessToken
                        -> WxppScene
                        -> m WxppMakeSceneResult
-- {{{1
wxppQrCodeCreatePersist atk scene = do
  wxppQrCodeCreateInternal
      [ "action_name" .= action ]
      atk
      scene
  where
    action = case scene of
               WxppSceneInt {} -> "QR_LIMIT_SCENE" :: Text
               WxppSceneStr {} -> "QR_LIMIT_STR_SCENE"
-- }}}1


-- | 创建短期场景二维码
-- UPDATE: 现在看文档又说可以用字串形式的场景参数
wxppQrCodeCreateTransient :: (WxppApiMonad env m)
                          => AccessToken
                          -> Int      -- ^ TTL in seconds
                          -> WxppScene
                          -> m WxppMakeSceneResult
-- {{{1
wxppQrCodeCreateTransient atk ttl scene =
  wxppQrCodeCreateInternal
      [ "action_name" .= action
      , "expire_seconds" .= ttl
      ]
      atk
      scene
  where
    action = case scene of
               WxppSceneInt {} -> "QR_SCENE" :: Text
               WxppSceneStr {} -> "QR_STR_SCENE"
-- }}}1


wxppQrCodeCreateInternal :: (WxppApiMonad env m)
                         => [Pair]
                         -> AccessToken
                         -> WxppScene
                         -> m WxppMakeSceneResult
wxppQrCodeCreateInternal js_pairs (AccessToken { accessTokenData = atk }) scene = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf ++ "/qrcode/create"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ toJSON $ object $
                ( "action_info" .= object [ "scene" .= scene ] ) : js_pairs)
        >>= asWxppWsResponseNormal'


-- | 可直接在网页上使用的显示二维码的URL
wxppQrCodeUrlByTicket :: QRTicket -> UrlText
wxppQrCodeUrlByTicket (QRTicket ticket) = UrlText $
    "https://mp.weixin.qq.com/cgi-bin/showqrcode"
        <> decodeUtf8 (BBB.toByteString $ renderQueryText True [ ("ticket", Just ticket ) ] )


-- vim: set foldmethod=marker:
