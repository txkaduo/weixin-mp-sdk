module WeiXin.PublicPlatform.MiniProgram where

import           ClassyPrelude
import           Control.Lens          hiding ((.=))
import           Data.Aeson            as A
import           Network.Wreq          hiding (Proxy)
import qualified Network.Wreq.Session  as WS
import           Yesod.Core

import Yesod.Helpers.Utils                  (nullToNothing)

import           WeiXin.PublicPlatform.Class
import           WeiXin.PublicPlatform.WS

data Code2SessionResult =
  Code2SessionResult
    { code2SessionOpenId  :: WxppOpenID
    , code2SessionSessionKey :: Text
    , code2SessionUnionId :: Maybe WxppUnionID
    }

instance FromJSON Code2SessionResult where
  parseJSON = withObject "Code2SessionResult" $ \o -> do
    Code2SessionResult
      <$> o .: "openid"
      <*> o .: "session_key"
      <*> (fmap WxppUnionID . join . fmap nullToNothing <$> o .:? "unionid")


wxppMpCode2Session :: ( WxppApiMonad env m )
                    => WxppAppID
                    -> WxppAppSecret
                    -> Text
                    -> m Code2SessionResult
wxppMpCode2Session app_id app_secret js_code = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSnsApiBase url_conf <> "/jscode2session"
        opts = defaults & param "grant_type" .~ [ "authorization_code" ]
                        & param "appid" .~ [ unWxppAppID app_id ]
                        & param "secret" .~ [ unWxppAppSecret app_secret ]
                        & param "js_code" .~ [ js_code ]

    sess' <- liftIO (WS.getWith opts sess url)
                >>= asWxppWsResponseNormal'
    return sess'

sessionKeyMiniProgram :: WxppAppID -> Text
sessionKeyMiniProgram app_id = "wxmp|" <> unWxppAppID app_id

sessionSetSessionKey :: MonadHandler m => WxppAppID -> Text -> m ()
sessionSetSessionKey app_id sk = setSession (sessionKeyMiniProgram app_id) sk

sessionHasSessionKey :: MonadHandler m => WxppAppID -> m Bool
sessionHasSessionKey app_id = fmap isJust $ lookupSession $ sessionKeyMiniProgram app_id
