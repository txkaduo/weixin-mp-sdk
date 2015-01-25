module WeiXin.PublicPlatform.Security
    ( module WeiXin.PublicPlatform.Security
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import qualified Data.ByteString            as B
import qualified Crypto.Hash.SHA1           as SHA1
import Data.Aeson                           ( FromJSON(..)
                                            , withObject, (.:))
import Control.Monad.Logger                 (MonadLogger, logDebugS)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS


wxppSignature :: Token -> TimeStampS -> Nonce -> Text -> ByteString
wxppSignature (Token token) (TimeStampS tt) (Nonce nn) msg =
    SHA1.hash $ encodeUtf8 $ mconcat $ sort [tt, nn, token, msg]

data AccessTokenResp = AccessTokenResp
                            AccessToken
                            Int

instance FromJSON AccessTokenResp where
    parseJSON = withObject "AccessTokenResp" $ \obj -> do
                    atk <- fmap AccessToken $ obj .: "access_token"
                    expiry <- obj .: "expires_in"
                    return $ AccessTokenResp atk expiry


-- | Refresh/update access token from WeiXin server.
-- May throw: WxppWsCallError
refreshAccessToken ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    WxppAppConfig -> m AccessTokenResp
refreshAccessToken wac = do
    let url = wxppRemoteApiBaseUrl <> "/token"
        opts = defaults & param "grant_type" .~ [ "client_credential" ]
                        & param "appid" .~ [ unWxppAppID app_id ]
                        & param "secret" .~ [ unWxppAppSecret app_secret ]
    atk <- (liftIO $ getWith opts url)
                >>= asWxppWsResponseNormal'
    $(logDebugS) wxppLogSource $ "access token has been refreshed."
    return atk
    where
        app_id      = wxppConfigAppID wac
        app_secret  = wxppConfigAppSecret wac


pkcs7PaddingEncode :: Int -> ByteString -> ByteString
pkcs7PaddingEncode blk_size bs =
    if pad_num <= 0
        then bs
        else bs <> (replicate pad_num $ fromIntegral pad_num)
    where
        blen    = length bs
        pad_num = blen `rem` blk_size


pkcs7PaddingDecode :: Int -> ByteString -> ByteString
pkcs7PaddingDecode blk_size bs =
    if null bs
        then bs
        else
            let pad_num = fromIntegral $ B.last bs
                pad_num' = if pad_num < 1 || pad_num > blk_size
                                then 0
                                else pad_num
            in flip take bs $ length bs - pad_num'
