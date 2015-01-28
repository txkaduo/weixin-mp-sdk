module WeiXin.PublicPlatform.Types where

import ClassyPrelude
import Data.SafeCopy
import Data.Aeson
import Data.Aeson.Types                     (Parser)
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import Data.Byteable                        (toBytes)
import Crypto.Cipher                        (makeKey, Key)
import Crypto.Cipher.AES                    (AES)

import Yesod.Helpers.Aeson                  (parseBase64ByteString)


newtype WxppMediaID = WxppMediaID { unWxppMediaID :: Text }
                        deriving (Show, Eq, Ord)

newtype WxppOpenID = WxppOpenID { unWxppOpenID :: Text}
                    deriving (Show, Eq, Ord)

newtype WxppInMsgID = WxppInMsgID { unWxppInMsgID :: Word64 }
                    deriving (Show, Eq, Ord)

newtype WxppSceneID = WxppSceneID { unWxppSceneID :: Word32 }
                    deriving (Show, Eq, Ord)

newtype QRTicket = QRTicket { unQRTicket :: Text }
                    deriving (Show, Eq, Ord)

newtype Token = Token { unToken :: Text }
                    deriving (Show, Eq, Ord)

newtype AesKey = AesKey { unAesKey :: Key AES }
                    deriving (Eq)
instance Show AesKey where
    show (AesKey k) = "AesKey:" <> (C8.unpack $ B64.encode $ toBytes k)

instance FromJSON AesKey where
    parseJSON = fmap AesKey .
                    (withText "AesKey" $ \t -> do
                        bs <- parseBase64ByteString "AesKey" t
                        either (fail . show) return $ makeKey bs
                    )

newtype TimeStampS = TimeStampS { unTimeStampS :: Text }
                    deriving (Show, Eq)

newtype Nonce = Nonce { unNounce :: Text }
                    deriving (Show, Eq)

newtype AccessToken = AccessToken { unAccessToken :: Text }
                    deriving (Show, Eq, Typeable)
$(deriveSafeCopy 0 'base ''AccessToken)


newtype WxppAppID = WxppAppID { unWxppAppID :: Text }
                    deriving (Show, Eq)

newtype WxppAppSecret = WxppAppSecret { unWxppAppSecret :: Text }
                    deriving (Show, Eq)

data WxppAppConfig = WxppAppConfig {
                    wxppConfigAppID         :: WxppAppID
                    , wxppConfigAppSecret   :: WxppAppSecret
                    , wxppConfigAppToken    :: Token
                    , wxppConfigAppAesKey   :: AesKey
                    , wxppConfigAppBackupAesKeys  :: [AesKey]
                        -- ^ 多个 aes key 是为了过渡时使用
                        -- 加密时仅使用第一个
                        -- 解密时则则所有都试一次
                    }
                    deriving (Show, Eq)

instance FromJSON WxppAppConfig where
    parseJSON = withObject "WxppAppConfig" $ \obj -> do
                    app_id <- fmap WxppAppID $ obj .: "app-id"
                    secret <- fmap WxppAppSecret $ obj .: "secret"
                    token <- fmap Token $ obj .: "token"
                    aes_key_lst <- obj .: "aes-key"
                    case aes_key_lst of
                        []      -> fail $ "At least one AesKey is required"
                        (x:xs)  ->
                            return $ WxppAppConfig app_id secret token
                                        x
                                        xs


data WxppEvent = WxppEvtSubscribe
                | WxppEvtUnsubscribe
                | WxppEvtSubscribeAtScene WxppSceneID QRTicket
                | WxppEvtScan WxppSceneID QRTicket
                | WxppEvtReportLocation (Double, Double) Double
                    -- ^ (纬度，经度） 精度
                | WxppEvtClickItem Text
                | WxppEvtFollowUrl Text
                deriving (Show, Eq)

data WxppInMsg =  WxppInMsgText Text
                | WxppInMsgImage WxppMediaID Text
                | WxppInMsgVoice WxppMediaID Text (Maybe Text)
                | WxppInMsgVideo WxppMediaID WxppMediaID
                | WxppInMsgLocation (Double, Double) Double Text
                    -- ^ (latitude, longitude) scale label
                | WxppInMsgLink Text Text Text
                    -- ^ url title description
                | WxppInMsgEvent WxppEvent
                deriving (Show, Eq)


data WxppInMsgEntity = WxppInMsgEntity
                        {
                            wxppInToUserName        :: Text
                            , wxppInFromUserName    :: WxppOpenID
                            , wxppInCreatedTime     :: UTCTime
                            , wxppInMessageID       :: WxppInMsgID
                            , wxppInMessage         :: WxppInMsg
                        }
                        deriving (Show, Eq)

-- | 图文信息
data WxppArticle = WxppArticle {
                    wxppArticleTitle    :: Maybe Text    -- ^ title
                    , wxppArticleDesc   :: Maybe Text    -- ^ description
                    , wxppArticlePicUrl :: Maybe Text    -- ^ pic url
                    , wxppArticleUrl    :: Maybe Text    -- ^ url
                    }
                    deriving (Show, Eq)

data WxppOutMsg = WxppOutMsgText Text
                | WxppOutMsgImage WxppMediaID
                | WxppOutMsgVoice WxppMediaID
                | WxppOutMsgVideo WxppMediaID (Maybe Text) (Maybe Text)
                    -- ^ media_id title description
                | WxppOutMsgMusic WxppMediaID (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)
                    -- ^ thumb_media_id, title, description, url, hq_url
                | WxppOutMsgArticle [WxppArticle]
                    -- ^ 根据文档，图文总数不可超过10
                        deriving (Show, Eq)

data WxppOutMsgEntity = WxppOutMsgEntity
                        {
                            wxppOutToUserName       :: WxppOpenID
                            , wxppOutFromUserName   :: Text
                            , wxppOutCreatedTime    :: UTCTime
                            , wxppOutMessage        :: WxppOutMsg
                        }
                        deriving (Show, Eq)


-- | 响应收到的服务器信息
-- Left 用于表达错误
-- Right Nothing 代表无需回复一个新信息
type WxppInMsgHandler m = WxppAppConfig
                            -> WxppInMsgEntity
                            -> m (Either String (Maybe WxppOutMsg))

class FromJsonHandler h where
    -- | 假定每个算法的配置段都有一个 name 的字段
    -- 根据这个方法选择出一个指定算法类型，
    -- 然后从 json 数据中反解出相应的值
    isNameOfInMsgHandler :: Monad n => n h -> Text -> Bool

    parseInMsgHandler :: Monad n => n h -> Object -> Parser h

-- | WxppInMsgHandler that can be parsed from JSON value
class FromJsonHandler h => JsonWxppInMsgHandler m h where
    handleInMsg :: h -> WxppInMsgHandler m


data SomeJsonWxppInMsgHandler m =
        forall h. JsonWxppInMsgHandler m h => SomeJsonWxppInMsgHandler h


wxppLogSource :: IsString a => a
wxppLogSource = "WXPP"
