module WeiXin.PublicPlatform.JS where

import ClassyPrelude hiding (catch)
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Base16     as B16
import qualified Data.Text                  as T
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens
import Data.Aeson
import Data.Time                            (NominalDiffTime, addUTCTime)
import Data.Time.Clock.POSIX                (getPOSIXTime)
import Text.Julius                          (julius, JavascriptUrl)
import Control.Monad.Logger

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Security


data JsTicketResult = JsTicketResult
                        WxppJsTicket
                        NominalDiffTime

instance FromJSON JsTicketResult where
    parseJSON = withObject "JsTicketResult" $ \o -> do
                    JsTicketResult
                        <$> o .: "ticket"
                        <*> ((fromIntegral :: Int -> NominalDiffTime) <$> o .: "expires_in")

wxppGetJsTicket :: (WxppApiMonad m)
                => AccessToken
                -> m JsTicketResult
wxppGetJsTicket (AccessToken atk _app_id) = do
    let url = wxppRemoteApiBaseUrl <> "/ticket/getticket"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "type" .~ [ "jsapi" ]

    sess <- ask
    liftM snd $ liftIO (WS.getWith opts sess url) >>= asWxppWsResponseNormal2'


wxppJsApiSignature :: WxppJsTicket
                    -> UrlText
                    -> Int64
                    -> Nonce         -- ^ random string
                    -> ByteString
wxppJsApiSignature (WxppJsTicket ticket) (UrlText url) ptime (Nonce noncestr) =
    SHA1.hash $ encodeUtf8 s_input
    where
        vars = sort [ ("jsapi_ticket", ticket)
                    , ("noncestr", noncestr)
                    , ("timestamp", tshow ptime)
                    , ("url", url)
                    ]
        s_input = intercalate "&" $ flip map vars $ \(k, v) -> k <> "=" <> v


wxppJsApiSignatureIO :: MonadIO m
                        => WxppJsTicket
                        -> UrlText
                        -> m (ByteString, (Int64, Nonce))
wxppJsApiSignatureIO ticket url = liftIO $ do
    nonce <- wxppMakeNonce 16   
    ptime <- getPOSIXTime
    let ptime' = round ptime
    let sign = wxppJsApiSignature ticket url ptime' nonce
    return (sign, (ptime', nonce))


-- | 因为目前没找到可能的方法在服务器端能确保取出当前页面的URL
-- yesod-helpers 的 getCurrentUrl 只是一个能回到相同的 route 的 url
-- (它依赖配置文件而变）
-- 目前的解释办法是：调用者（服务器端）提供一个它认为正确的url
-- js 里（客户端）判断当前页面与服务器预期的不一致的话，
-- 则重定向至服务器预期的地址上去
wxppJsApiConfig :: MonadIO m
                => WxppAppID
                -> WxppJsTicket
                -> Bool
                -> UrlText
                -> [Text]   -- ^ API list
                -> m (JavascriptUrl url)
wxppJsApiConfig app_id ticket debug url api_list = do
    (sign, (ptime, Nonce nonce)) <- wxppJsApiSignatureIO ticket url
    return $ [julius|
        function get_hashless_url() {
            var url = window.location.href;
            var hash = window.location.hash;
            var index_of_hash = url.lastIndexOf(hash);
            if (index_of_hash > 0)
            {
                return url.substr(0, index_of_hash);
            } else {
                return url;
            }
        }

        if (#{toJSON $ unUrlText url} != get_hashless_url()) {
            window.location = #{toJSON $ unUrlText url};
        }

        wx.config({ debug: #{toJSON debug}, // 开启调试模式,调用的所有api的返回值会在客户端alert出来，若要查看传入的参数，可以在pc端打开，参数信息会通过log打出，仅在pc端时才会打印。 
            appId: #{toJSON app_id}, // 必填，公众号的唯一标识
            timestamp: #{toJSON ptime}, // 必填，生成签名的时间戳
            nonceStr: #{toJSON nonce}, // 必填，生成签名的随机串
            signature: #{toJSON $ T.toLower $ fromString $ C8.unpack $ B16.encode sign},// 必填，签名，见附录1
            jsApiList: #{toJSON api_list} // 必填，需要使用的JS接口列表，所有JS接口列表见附录2
        });|]


wxppAcquireAndSaveJsApiTicket :: ( WxppApiMonad m, MonadCatch m
                                , WxppCacheTokenUpdater c, WxppCacheTokenReader c
                                )
                                => c
                                -> WxppAppID
                                -> m ()
wxppAcquireAndSaveJsApiTicket cache app_id = do
    now <- liftIO getCurrentTime
    m_atk_info <- wxppGetUsableAccessToken cache app_id
    case m_atk_info of
        Nothing -> do
            $logErrorS wxppLogSource $
                "cannot refresh js ticket because no access token is available, app_id="
                <> unWxppAppID app_id

        Just (atk, _) -> do
            JsTicketResult ticket ttl <- wxppGetJsTicket atk
            let expiry = addUTCTime ttl now
            liftIO $ wxppCacheAddJsTicket cache app_id ticket expiry
            $logDebugS wxppLogSource $
                "JS ticket refreshed, app_id=" <> unWxppAppID app_id


wxppJsSDKUrl :: UrlText
wxppJsSDKUrl = UrlText $ "http://res.wx.qq.com/open/js/jweixin-1.1.0.js"

wxppJsApiListAll :: [Text]
wxppJsApiListAll =
    [ "onMenuShareTimeline"
    , "onMenuShareAppMessage"
    , "onMenuShareQQ"
    , "onMenuShareWeibo"
    , "onMenuShareQZone"
    , "startRecord"
    , "stopRecord"
    , "onVoiceRecordEnd"
    , "playVoice"
    , "pauseVoice"
    , "stopVoice"
    , "onVoicePlayEnd"
    , "uploadVoice"
    , "downloadVoice"
    , "chooseImage"
    , "previewImage"
    , "uploadImage"
    , "downloadImage"
    , "translateVoice"
    , "getNetworkType"
    , "openLocation"
    , "getLocation"
    , "hideOptionMenu"
    , "showOptionMenu"
    , "hideMenuItems"
    , "showMenuItems"
    , "hideAllNonBaseMenuItem"
    , "showAllNonBaseMenuItem"
    , "closeWindow"
    , "scanQRCode"
    , "chooseWXPay"
    , "openProductSpecificView"
    , "addCard"
    , "chooseCard"
    , "openCard"
    ]
