module WeiXin.PublicPlatform.JS where

import ClassyPrelude hiding (catch)
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Time                            (NominalDiffTime)
-- import Control.Monad.Logger

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS


data JsTicketResult = JsTicketResult
                        WxppJsTicket
                        NominalDiffTime

instance FromJSON JsTicketResult where
    parseJSON = withObject "JsTicketResult" $ \o -> do
                    JsTicketResult
                        <$> o .: "ticket"
                        <*> ((fromIntegral :: Int -> NominalDiffTime) <$> o .: "expires_in")

wxppGetJsTicket :: (MonadIO m, MonadThrow m)
                => AccessToken
                -> m JsTicketResult
wxppGetJsTicket (AccessToken atk _app_id) = do
    let url = wxppRemoteApiBaseUrl <> "/ticket/getticket"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "type" .~ [ "jsapi" ]
    liftM snd $ liftIO (getWith opts url) >>= asWxppWsResponseNormal2'
