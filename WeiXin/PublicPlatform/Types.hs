{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Types
    ( module WeiXin.PublicPlatform.Types
    , Gender(..)
    , UrlText(..)
    ) where

import ClassyPrelude
import Data.SafeCopy
import Data.Aeson                           as A
import qualified Data.Text                  as T
import Data.Aeson.Types                     (Parser, Pair, typeMismatch)
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB
import Data.Byteable                        (toBytes)
import Crypto.Cipher                        (makeKey, Key)
import Crypto.Cipher.AES                    (AES)
import Data.Time                            (addUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX                ( posixSecondsToUTCTime
                                            , utcTimeToPOSIXSeconds)
import Data.Scientific                      (toBoundedInteger)
import Text.Read                            (reads)
import Filesystem.Path.CurrentOS            (encodeString, fromText)
import qualified Crypto.Hash.MD5            as MD5
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..)
                                            , SqlType(SqlString))

import Yesod.Helpers.Aeson                  (parseBase64ByteString)
import Yesod.Helpers.Types                  (Gender(..), UrlText(..), unUrlText)


epochIntToUtcTime :: Int64 -> UTCTime
epochIntToUtcTime = posixSecondsToUTCTime . (realToFrac :: Int64 -> NominalDiffTime)

newtype WxppMediaID = WxppMediaID { unWxppMediaID :: Text }
                        deriving (Show, Eq, Ord)

instance SafeCopy WxppMediaID where
    getCopy                 = contain $ safeGet
    putCopy (WxppMediaID x) = contain $ safePut x

newtype WxppOpenID = WxppOpenID { unWxppOpenID :: Text}
                    deriving (Show, Eq, Ord)

instance PersistField WxppOpenID where
    toPersistValue      = toPersistValue . unWxppOpenID
    fromPersistValue    = fmap WxppOpenID . fromPersistValue

instance PersistFieldSql WxppOpenID where
    sqlType _ = SqlString

newtype WxppInMsgID = WxppInMsgID { unWxppInMsgID :: Word64 }
                    deriving (Show, Eq, Ord)

instance PersistField WxppInMsgID where
    toPersistValue      = toPersistValue . unWxppInMsgID
    fromPersistValue    = fmap WxppInMsgID . fromPersistValue

instance PersistFieldSql WxppInMsgID where
    sqlType _ = SqlString

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
                | WxppInMsgImage WxppMediaID UrlText
                | WxppInMsgVoice WxppMediaID Text (Maybe Text)
                | WxppInMsgVideo WxppMediaID WxppMediaID
                | WxppInMsgLocation (Double, Double) Double Text
                    -- ^ (latitude, longitude) scale label
                | WxppInMsgLink UrlText Text Text
                    -- ^ url title description
                | WxppInMsgEvent WxppEvent
                deriving (Show, Eq)


data WxppInMsgEntity = WxppInMsgEntity
                        {
                            wxppInToUserName        :: Text
                            , wxppInFromUserName    :: WxppOpenID
                            , wxppInCreatedTime     :: UTCTime
                            , wxppInMessageID       :: Maybe WxppInMsgID
                                -- ^ 从文档看，除了事件通知，所有信息都有 MsgID
                            , wxppInMessage         :: WxppInMsg
                        }
                        deriving (Show, Eq)

-- | 图文信息
data WxppArticle = WxppArticle {
                    wxppArticleTitle    :: Maybe Text    -- ^ title
                    , wxppArticleDesc   :: Maybe Text    -- ^ description
                    , wxppArticlePicUrl :: Maybe UrlText -- ^ pic url
                    , wxppArticleUrl    :: Maybe UrlText -- ^ url
                    }
                    deriving (Show, Eq)

instance FromJSON WxppArticle where
    parseJSON = withObject "WxppArticle" $ \obj -> do
                title <- obj .:? "title"
                desc <- obj .:? "desc"
                pic_url <- fmap UrlText <$> obj .:? "pic-url"
                url <- fmap UrlText <$> obj .:? "url"
                return $ WxppArticle title desc pic_url url

-- | 外发的信息
data WxppOutMsg = WxppOutMsgText Text
                | WxppOutMsgImage WxppMediaID
                | WxppOutMsgVoice WxppMediaID
                | WxppOutMsgVideo WxppMediaID (Maybe Text) (Maybe Text)
                    -- ^ media_id title description
                | WxppOutMsgMusic WxppMediaID (Maybe Text) (Maybe Text) (Maybe UrlText) (Maybe UrlText)
                    -- ^ thumb_media_id, title, description, url, hq_url
                | WxppOutMsgArticle [WxppArticle]
                    -- ^ 根据文档，图文总数不可超过10
                deriving (Show, Eq)

-- | 外发的信息的本地信息
-- 因 WxppOutMsg 包含 media id，它只在上传3天内有效，这个类型的值代表的就是相应的本地长期有效的信息
data WxppOutMsgL = WxppOutMsgTextL Text
                | WxppOutMsgImageL FilePath
                | WxppOutMsgVoiceL FilePath
                | WxppOutMsgVideoL FilePath (Maybe Text) (Maybe Text)
                    -- ^ media_id title description
                | WxppOutMsgMusicL FilePath (Maybe Text) (Maybe Text) (Maybe UrlText) (Maybe UrlText)
                    -- ^ thumb_media_id, title, description, url, hq_url
                | WxppOutMsgArticleL [WxppArticle]
                    -- ^ 根据文档，图文总数不可超过10
                deriving (Show, Eq)

instance FromJSON WxppOutMsgL where
    parseJSON = withObject "WxppOutMsgL" $ \obj -> do
                    type_s <- obj .: "type"
                    case type_s of
                        "text" -> WxppOutMsgTextL <$> obj .: "text"
                        "image" -> WxppOutMsgImageL . fromText <$> obj .: "path"
                        "voice" -> WxppOutMsgVoiceL . fromText <$> obj .: "path"
                        "video" -> do
                                    path <- fromText <$> obj .: "path"
                                    title <- obj .:? "title"
                                    desc <- obj .:? "desc"
                                    return $ WxppOutMsgVideoL path title desc
                        "music" -> do
                                    path <- fromText <$> obj .: "path"
                                    title <- obj .:? "title"
                                    desc <- obj .:? "desc"
                                    url <- fmap UrlText <$> obj .:? "url"
                                    hq_url <- fmap UrlText <$> obj .:? "hq-url"
                                    return $ WxppOutMsgMusicL path title desc url hq_url
                        "article" -> WxppOutMsgArticleL <$> obj .: "articles"
                        _       -> fail $ "unknown type: " <> type_s


data WxppMediaType = WxppMediaTypeImage
                    | WxppMediaTypeVoice
                    | WxppMediaTypeVideo
                    | WxppMediaTypeThumb
                    deriving (Show, Eq, Ord, Enum, Bounded)

deriveSafeCopy 0 'base ''WxppMediaType

data WxppOutMsgEntity = WxppOutMsgEntity
                        {
                            wxppOutToUserName       :: WxppOpenID
                            , wxppOutFromUserName   :: Text
                            , wxppOutCreatedTime    :: UTCTime
                            , wxppOutMessage        :: WxppOutMsg
                        }
                        deriving (Show, Eq)



-- | 可以点击的菜单所携带的数据及菜单的类型
-- 虽然看上去所有菜单都只有个文本作为数据，但概念上这些文本并不相同
-- 例如，有时候它明确地就是一个 URL
data MenuItemData = MenuItemDataClick Text
                        -- ^ key
                    | MenuItemDataView UrlText
                        -- ^ url
                    | MenuItemDataScanCode Text
                        -- ^ key
                    | MenuItemDataScanCodeWaitMsg Text
                        -- ^ key
                    | MenuItemDataPicSysPhoto Text
                        -- ^ key
                    | MenuItemDataPicPhotoOrAlbum Text
                        -- ^ key
                    | MenuItemDataPicWeiXin Text
                        -- ^ key
                    | MenuItemDataLocationSelect Text
                        -- ^ key
                    deriving (Show, Eq)


menuItemDataToJsonPairs :: MenuItemData -> [Pair]
menuItemDataToJsonPairs (MenuItemDataClick key) =
                                [ "type"    .= ("click" :: Text)
                                , "key"     .= key
                                ]
menuItemDataToJsonPairs (MenuItemDataView (UrlText url)) =
                                [ "type"    .= ("view" :: Text)
                                , "url"     .= url
                                ]
menuItemDataToJsonPairs (MenuItemDataScanCode key) =
                                [ "type"    .= ("scancode_push" :: Text)
                                , "key"     .= key
                                ]
menuItemDataToJsonPairs (MenuItemDataScanCodeWaitMsg key) =
                                [ "type"    .= ("scancode_waitmsg" :: Text)
                                , "key"     .= key
                                ]
menuItemDataToJsonPairs (MenuItemDataPicSysPhoto key) =
                                [ "type"    .= ("pic_sysphoto" :: Text)
                                , "key"     .= key
                                ]
menuItemDataToJsonPairs (MenuItemDataPicPhotoOrAlbum key) =
                                [ "type"    .= ("pic_photo_or_album" :: Text)
                                , "key"     .= key
                                ]
menuItemDataToJsonPairs (MenuItemDataPicWeiXin key) =
                                [ "type"    .= ("pic_weixin" :: Text)
                                , "key"     .= key
                                ]
menuItemDataToJsonPairs (MenuItemDataLocationSelect key) =
                                [ "type"    .= ("location_select" :: Text)
                                , "key"     .= key
                                ]

menuItemDataFromJsonObj :: Object -> Parser MenuItemData
menuItemDataFromJsonObj obj = do
    typ <- obj .: "type"
    case typ of
        "click"             -> fmap MenuItemDataClick $ obj .: "key"
        "view"              -> fmap MenuItemDataView $ UrlText <$> obj .: "url"
        "scancode_push"     -> fmap MenuItemDataScanCode $ obj .: "key"
        "scancode_waitmsg"  -> fmap MenuItemDataScanCodeWaitMsg $ obj .: "key"
        "pic_sysphoto"      -> fmap MenuItemDataPicSysPhoto $ obj .: "key"
        "pic_photo_or_album"-> fmap MenuItemDataPicPhotoOrAlbum $ obj .: "key"
        "pic_weixin"        -> fmap MenuItemDataPicWeiXin $ obj .: "key"
        "location_select"   -> fmap MenuItemDataLocationSelect $ obj .: "key"
        _                   -> fail $ "unknown/unsupported menu type: " <> typ


-- | 菜单项，及其子菜单
data MenuItem = MenuItem {
                    menuItemName             :: Text
                    , menuItemDataOrSubs    :: Either MenuItemData [MenuItem]
                }
                deriving (Show, Eq)

instance ToJSON MenuItem where
    toJSON mi = object $
                    [ "name" .= menuItemName mi ]
                    ++
                        either
                            menuItemDataToJsonPairs
                            (\items -> [("sub_button" .= map toJSON items)])
                            (menuItemDataOrSubs mi)


instance FromJSON MenuItem where
    parseJSON = withObject "MenuItem" $ \obj -> do
                    name <- obj .: "name"
                    m_subs <- obj .:? "sub_button"
                    dat_or_subs <- case m_subs of
                        Just (subs@(_:_))   -> return $ Right subs
                        _                   -> fmap Left $ menuItemDataFromJsonObj obj
                    return $ MenuItem name dat_or_subs


newtype WxppUnionID = WxppUnionID { unWxppUnionID :: Text }
                    deriving (Show, Eq, Ord)

data SimpleLocaleName = SimpleLocaleName { unSimpleLocaleName :: Text }
                    deriving (Show, Eq, Ord)

type NickName = Text
type CityName = Text
type ProvinceName = Text
type ContryName = Text

-- | 用户基础信息查询接口的返回
data EndUserQueryResult = EndUserQueryResultNotSubscribed WxppOpenID
                        | EndUserQueryResult
                            WxppOpenID
                            NickName    -- ^ nickname
                            (Maybe Gender)
                            SimpleLocaleName
                            CityName
                            ProvinceName
                            ContryName
                            UrlText     -- ^ head image url
                            UTCTime
                            (Maybe WxppUnionID)
                        deriving (Show, Eq, Ord)

instance FromJSON EndUserQueryResult where
    parseJSON = withObject "EndUserQueryResult" $ \obj -> do
                open_id <- WxppOpenID <$> obj .: "openid"
                if_sub <- (\x -> x > (0 :: Int)) <$> obj .: "subscribe"
                if not if_sub
                    then return $ EndUserQueryResultNotSubscribed open_id
                    else do
                        nickname <- obj .: "nickname"
                        m_gender <- obj .: "sex" >>= parseSexJson
                        city <- obj .: "city"
                        lang <- SimpleLocaleName <$> obj .: "language"
                        province <- obj .: "province"
                        contry <- obj .: "contry"
                        headimgurl <- UrlText <$> obj .: "headimgurl"
                        subs_time <- epochIntToUtcTime <$> obj .: "subscribe_time"
                        m_union_id <- fmap WxppUnionID <$> obj .:? "unionid"
                        return $ EndUserQueryResult
                                    open_id
                                    nickname
                                    m_gender
                                    lang
                                    city
                                    province
                                    contry
                                    headimgurl
                                    subs_time
                                    m_union_id

-- | sex 字段出现在文档两处，有时候是个整数，有时候是个字串
-- 这个函数处理两种情况
parseSexJson :: Value -> Parser (Maybe Gender)
parseSexJson = go
    where
        go (A.String t) = p t $ fmap fst $ listToMaybe $ reads $ T.unpack t
        go (A.Number n) = p n $ toBoundedInteger n
        go v            = typeMismatch "Number or String" v

        p :: Show a => a -> Maybe Int -> Parser (Maybe Gender)
        p x mi = case mi of
                Just i  -> parseSexInt i
                Nothing -> fail $ "unknown sex: " <> show x


parseSexInt :: Monad m => Int -> m (Maybe Gender)
parseSexInt 0 = return Nothing
parseSexInt 1 = return $ Just Male
parseSexInt 2 = return $ Just Female
parseSexInt x = fail $ "unknown sex: " <> show x


newtype MD5Hash = MD5Hash { unMD5Hash :: ByteString }
                deriving (Show, Eq, Ord)

instance SafeCopy MD5Hash where
    getCopy             = contain $ safeGet
    putCopy (MD5Hash x) = contain $ safePut x

-- | 上传媒体文件的结果
data UploadResult = UploadResult {
                        urMediaType     :: WxppMediaType
                        , urMediaId     :: WxppMediaID
                        , urCreateTime  :: UTCTime
                        }
                        deriving (Show, Typeable)

deriveSafeCopy 0 'base ''UploadResult

instance FromJSON UploadResult where
    parseJSON = withObject "UploadResult" $ \obj -> do
        type_s <- obj .: "type"
        typ <- case type_s of
                "image" -> return WxppMediaTypeImage
                "voice" -> return WxppMediaTypeVoice
                "video" -> return WxppMediaTypeVideo
                "thumb" -> return WxppMediaTypeThumb
                _       -> fail $ "unknown type: " <> type_s
        media_id <- WxppMediaID <$> obj .: "media_id"
        t <- epochIntToUtcTime <$> obj .: "created_at"
        return $ UploadResult typ media_id t

--------------------------------------------------------------------------------

utcTimeToEpochInt :: UTCTime -> Int64
utcTimeToEpochInt = round . utcTimeToPOSIXSeconds

wxppLogSource :: IsString a => a
wxppLogSource = "WXPP"

md5HashFile :: FilePath -> IO MD5Hash
md5HashFile = fmap (MD5Hash . MD5.hashlazy) . LB.readFile . encodeString

-- | 上传得到的 media id 只能用一段时间
usableUploadResult :: UTCTime -> NominalDiffTime -> UploadResult -> Bool
usableUploadResult now dt ur = addUTCTime dt (urCreateTime ur) < now
