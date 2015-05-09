module WeiXin.PublicPlatform.Security
    ( module WeiXin.PublicPlatform.Security
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Data.Word
import Data.Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Builder    as BB
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.Text                  as T
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import Data.Aeson                           ( FromJSON(..)
                                            , withObject, (.:))
import Crypto.Cipher                        ( makeIV, IV, cbcEncrypt
                                            , cbcDecrypt, cipherInit)
import Crypto.Cipher.AES                    (AES)
import Control.Monad.Logger
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import System.Random                        (randomIO)
import Data.Byteable                        (toBytes)
import qualified Data.Attoparsec.ByteString as AttoB

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
refreshAccessToken ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    WxppAppConfig -> m AccessTokenResp
refreshAccessToken wac = refreshAccessToken' app_id app_secret
    where
        app_id      = wxppConfigAppID wac
        app_secret  = wxppConfigAppSecret wac

refreshAccessToken' ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    WxppAppID
    -> WxppAppSecret
    -> m AccessTokenResp
refreshAccessToken' app_id app_secret = do
    let url = wxppRemoteApiBaseUrl <> "/token"
        opts = defaults & param "grant_type" .~ [ "client_credential" ]
                        & param "appid" .~ [ unWxppAppID app_id ]
                        & param "secret" .~ [ unWxppAppSecret app_secret ]
    atk <- (liftIO $ getWith opts url)
                >>= asWxppWsResponseNormal'
    $(logDebugS) wxppLogSource $ "access token has been refreshed."
    return atk



pkcs7PaddingEncode :: Int -> ByteString -> ByteString
pkcs7PaddingEncode blk_size bs =
    if pad_num <= 0
        then bs
        else bs <> (replicate pad_num $ fromIntegral pad_num)
    where
        blen    = length bs
        pad_num = blk_size - blen `rem` blk_size


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


-- | 加密信息的核心算法
wxppEncryptInternal ::
    WxppAppID
    -> AesKey
    -> IV AES        -- ^ IV
    -> ByteString       -- ^ salt to prepend to msg
    -> ByteString       -- ^ message
    -> ByteString
wxppEncryptInternal (WxppAppID app_id) (AesKey ak) iv salt msg =
    cbcEncrypt ctx iv $ pkcs7PaddingEncode 32 plain_bs
    where
        ctx     = cipherInit ak
        msg_len = fromIntegral $ length msg
        plain_bs = LB.toStrict $ BB.toLazyByteString $ mconcat plain
        plain   =   [ BB.byteString $ take 16 salt
                    , BB.word32BE msg_len
                    , BB.byteString msg
                    , BB.stringUtf8 $ T.unpack app_id
                    ]


-- | 文档没有明确写，但示范代码就是使用 aes key的前16字节
-- 这个函数体现了这个逻辑
wxppEncryptInternal2 ::
    WxppAppID
    -> AesKey
    -> ByteString       -- ^ salt to prepend to msg
    -> ByteString       -- ^ message
    -> Either String ByteString
wxppEncryptInternal2 app_id ak salt msg = do
    case makeIV iv_bs of
        Nothing -> do
                -- $(logErrorS) wxppLogSource $ "cannot make IV"
                Left $ "cannot make IV"
        Just iv -> do
                return $ wxppEncryptInternal app_id ak iv salt msg
    where
        iv_bs = take 16 $ toBytes $ unAesKey ak

-- | 加密明文的入口
wxppEncrypt :: MonadIO m =>
    WxppAppID
    -> AesKey
    -> ByteString       -- ^ message
    -> m (Either String ByteString)
wxppEncrypt app_id ak msg = do
    salt <- liftIO $ liftM B.pack $ replicateM 16 randomIO
    return $ wxppEncryptInternal2 app_id ak salt msg


-- | 加密明文的入口: base64-encoded
wxppEncryptB64 :: MonadIO m =>
    WxppAppID
    -> AesKey
    -> ByteString       -- ^ message
    -> m (Either String ByteString)
wxppEncryptB64 app_id ak msg = runExceptT $ do
    liftM B64.encode $ ExceptT $ wxppEncrypt app_id ak msg

-- | 加密明文的入口: 文本到文本格式
wxppEncryptText :: MonadIO m =>
    WxppAppID
    -> AesKey
    -> Text       -- ^ message
    -> m (Either String Text)
wxppEncryptText app_id ak msg = runExceptT $ do
    liftM (T.pack . C8.unpack) $
        ExceptT $ wxppEncryptB64 app_id ak $ encodeUtf8 msg

wxppDecryptInternal ::
    WxppAppID
    -> AesKey
    -> IV AES        -- ^ IV
    -> ByteString       -- ^ encrypted message
    -> Either String ByteString
wxppDecryptInternal app_id (AesKey ak) iv encrypted =
    AttoB.parseOnly (wxppParseDecryptedBs app_id) plain
    where
        ctx     = cipherInit ak
        plain   = pkcs7PaddingDecode 32 $ cbcDecrypt ctx iv encrypted


wxppDecrypt ::
    WxppAppID
    -> AesKey
    -> ByteString       -- ^ encrypted message
    -> Either String ByteString
wxppDecrypt app_id ak encrypted = do
    iv <- maybe (Left "cannot make IV") return $ makeIV iv_bs
    wxppDecryptInternal app_id ak iv encrypted
    where
        -- 文档没有明确写，但示范代码就是使用 aes key的前16字节
        iv_bs = take 16 $ toBytes $ unAesKey ak


wxppParseDecryptedBs :: WxppAppID -> AttoB.Parser ByteString
wxppParseDecryptedBs (WxppAppID app_id) = do
    _salt <- AttoB.take 16
    msg_len <- fmap fromIntegral anyWord32BE
    msg_and_app_id <- AttoB.takeByteString
    let total_len = length msg_and_app_id
    when ( total_len < msg_len) $ do
        fail $ "does not end with AppID (too short)."
    let (msg, app_id2) = B.splitAt msg_len msg_and_app_id
    when (encodeUtf8 app_id /= app_id2) $ do
        fail $ "does not end with AppID: "
                <> C8.unpack app_id2
                <> "!=" <> (C8.unpack $ encodeUtf8 app_id)
    return msg


anyWord16BE :: AttoB.Parser Word16
anyWord16BE = do
    b1 <- AttoB.anyWord8
    b2 <- AttoB.anyWord8
    return $ fromIntegral b1 `shiftL` 8 + fromIntegral b2

anyWord32BE :: AttoB.Parser Word32
anyWord32BE = do
    x1 <- anyWord16BE
    x2 <- anyWord16BE
    return $ fromIntegral x1 `shiftL` 16 + fromIntegral x2

