{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
module WeiXin.PublicPlatform.Types
    ( module WeiXin.PublicPlatform.Types
    , Gender(..)
    , UrlText(..)
    ) where

-- {{{1
import ClassyPrelude hiding (try, optional)
import Control.DeepSeq                      (NFData)
import Data.SafeCopy
import Data.Aeson                           as A
import qualified Data.Text                  as T
import Data.Aeson.Types                     (Parser, Pair, typeMismatch)
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
-- import qualified Data.ByteString.Lazy       as LB
import qualified Data.Set                   as Set
import Data.Binary                          (Binary)
import Data.Binary.Orphans                  ()
import Data.Byteable                        (toBytes)
import Data.Char                            (isSpace, isAscii, isAlphaNum)
import Data.Default                         (Default(..))
import Data.Monoid                          (Endo(..))
import Crypto.Cipher                        (makeKey, Key)
import Crypto.Cipher.AES                    (AES)
import Data.Time                            (addUTCTime, NominalDiffTime)
import Data.Scientific                      (toBoundedInteger)
import Text.Read                            (reads)
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..))
import Database.Persist                     (PersistValue)
import Yesod.Core                           (PathPiece(..))
import Text.Read                            (Read(..))
import Data.Proxy                           (Proxy(..))
import Text.Shakespeare.I18N                (ToMessage(..))
import Text.Blaze.Html                      (ToMarkup(..))
import Language.Haskell.TH.Lift             (deriveLift)
import Web.HttpApiData                      (ToHttpApiData, FromHttpApiData)

import Yesod.Helpers.Aeson                  (parseArray, parseIntWithTextparsec, parseTextByParsec)
import Yesod.Helpers.Utils                  (nullToNothing)
import Yesod.Helpers.Types                  (Gender(..), UrlText(..), unUrlText)
import Text.Parsec.TX.Utils                 ( SimpleEncode(..), SimpleStringRep(..)
                                            , makeSimpleParserByTable, natural
                                            , derivePersistFieldS, deriveSimpleStringRepEnumBounded
                                            , deriveJsonS
                                            )
import Yesod.Helpers.Parsec                 ( derivePathPieceS )
import Text.Parsec
import qualified Data.HashMap.Strict        as HM
import Data.List.NonEmpty                   (NonEmpty(..), nonEmpty)


import WeiXin.PublicPlatform.Utils
-- }}}1


class ToEnumEither a where
  toEnumEither :: Int -> Either String a
  default toEnumEither :: Enum a => Int -> Either String a
  toEnumEither = Right . toEnum


data WxppUrlConfig = WxppUrlConfig
  { wxppUrlConfSecureApiBase    :: String
  , wxppUrlConfNonSecureApiBase :: String
  , wxppUrlConfSnsApiBase       :: String
  , wxppUrlConfFileApiBase      :: String
  , wxppUrlConfUserPayApiBase   :: String
  -- ^ 用户付款接口的接口基地址
  , wxppUrlConfUserPayApiSecBase :: String
  -- ^ 用户付款接口的中要双向证书的接口基地址
  -- 目前就只有申请退款的接口在这个目录下
  , wxppUrlConfMmPayApiBase     :: String
  -- ^ 企业付款接口的接口基地址
  }
  deriving (Show)

-- {{{1 instances for WxppUrlConfig
instance Default WxppUrlConfig where
  def = WxppUrlConfig
    { wxppUrlConfSecureApiBase    = "https://api.weixin.qq.com/cgi-bin"
    , wxppUrlConfNonSecureApiBase = "http://api.weixin.qq.com/cgi-bin"
    , wxppUrlConfSnsApiBase       = "https://api.weixin.qq.com/sns"
    , wxppUrlConfFileApiBase      = "https://file.api.weixin.qq.com/cgi-bin"
    , wxppUrlConfUserPayApiBase   = "https://api.mch.weixin.qq.com/pay"
    , wxppUrlConfUserPayApiSecBase = "https://api.mch.weixin.qq.com/secapi/pay"
    , wxppUrlConfMmPayApiBase     = "https://api.mch.weixin.qq.com/mmpaymkttransfers"
    }

-- | allow use JSON/Yaml to override any field of WxppUrlConfig
instance FromJSON WxppUrlConfig where
  parseJSON = withObject "WxppUrlConfig" $ \o -> do
    fmap (($ def) . appEndo . mconcat) $ sequenceA
      [ o .:? "secure-base"     >>= return . maybe mempty (Endo . upd_wxppUrlConfSecureBase) . chk_empty
      , o .:? "non-secure-base" >>= return . maybe mempty (Endo . upd_wxppUrlConfNonSecureBase) . chk_empty
      , o .:? "sns-base"        >>= return . maybe mempty (Endo . upd_wxppUrlConfSnsApiBase) . chk_empty
      , o .:? "file-base"       >>= return . maybe mempty (Endo . upd_wxppUrlConfFileApiBase) . chk_empty
      , o .:? "user-pay-base"   >>= return . maybe mempty (Endo . upd_wxppUrlConfUserPayApiBase) . chk_empty
      , o .:? "user-pay-sec-base" >>= return . maybe mempty (Endo . upd_wxppUrlConfUserPayApiSecBase) . chk_empty
      , o .:? "mm-pay-base"     >>= return . maybe mempty (Endo . upd_wxppUrlConfMmPayApiBase) . chk_empty
      ]
    where
        null_to_empty s                  = if null s then Nothing else Just s
        chk_empty                        = join . fmap null_to_empty
        upd_wxppUrlConfSecureBase x c    = c { wxppUrlConfSecureApiBase = x }
        upd_wxppUrlConfNonSecureBase x c = c { wxppUrlConfNonSecureApiBase = x }
        upd_wxppUrlConfSnsApiBase x c    = c { wxppUrlConfSnsApiBase = x }
        upd_wxppUrlConfFileApiBase x c   = c { wxppUrlConfFileApiBase = x }
        upd_wxppUrlConfUserPayApiBase x c = c { wxppUrlConfUserPayApiBase = x }
        upd_wxppUrlConfUserPayApiSecBase x c = c { wxppUrlConfUserPayApiSecBase = x }
        upd_wxppUrlConfMmPayApiBase x c  = c { wxppUrlConfMmPayApiBase = x }
-- }}}1


-- | 微信用户名
newtype WeixinUserName = WeixinUserName { unWeixinUserName :: Text }
                        deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql
                                 , NFData, Binary
                                 , ToMessage, ToMarkup
                                 )


-- | 用户分组的ID
newtype WxppUserGroupID = WxppUserGroupID { unWxppUserGroupID :: Int }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, NFData)


-- | 卡券ID
newtype WxCardID = WxCardID { unWxCardID :: Text }
                    deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql
                             , NFData
                             , ToMessage, ToMarkup
                             )

-- | 客服帐号
newtype WxppKfAccount = WxppKfAccount { unWxppKfAccount :: Text }
  deriving (Show, Eq, Ord, ToMessage, ToMarkup, NFData)


-- | 为区分临时素材和永久素材，这个值专指 临时素材
newtype WxppBriefMediaID = WxppBriefMediaID { unWxppBriefMediaID :: Text }
                        deriving (Show, Eq, Ord, Typeable, Generic, Binary
                                 , NFData
                                 , PersistField, PersistFieldSql
                                 , ToJSON
                                 , ToMessage, ToMarkup)

-- {{{1 instance
instance SafeCopy WxppBriefMediaID where
    getCopy                         = contain $ WxppBriefMediaID <$> safeGet
    putCopy (WxppBriefMediaID x)    = contain $ safePut x
    errorTypeName _                 = "WxppBriefMediaID"

instance FromJSON WxppBriefMediaID where
  parseJSON = fmap WxppBriefMediaID
                . (parseJSON >=> nonEmptyJsonText "Weixin brief media id cannot be empty text")
-- }}}1


-- | 为区分临时素材和永久素材，这个值专指 永久素材
-- 虽然文档叫这种值 media id，但接口用的词是 material
newtype WxppDurableMediaID = WxppDurableMediaID { unWxppDurableMediaID :: Text }
  deriving (Show, Eq, Ord, Read, ToMessage, ToMarkup, NFData
           , PersistField, PersistFieldSql
           , ToJSON
           )

-- {{{1 instances
instance SafeCopy WxppDurableMediaID where
    getCopy                         = contain $ WxppDurableMediaID <$> safeGet
    putCopy (WxppDurableMediaID x)  = contain $ safePut x
    errorTypeName _                 = "WxppDurableMediaID"

instance FromJSON WxppDurableMediaID where
  parseJSON = fmap WxppDurableMediaID
                . (parseJSON >=> nonEmptyJsonText "Weixin durable media id cannot be empty text")

instance PathPiece WxppDurableMediaID where
    fromPathPiece = fmap WxppDurableMediaID . fromPathPiece
    toPathPiece = toPathPiece . unWxppDurableMediaID
-- }}}1


-- | 代表永久或临时的素材ID
newtype WxppMediaID = WxppMediaID { unWxppMediaID :: Text }
                    deriving (Show, Eq, Typeable, Generic, Binary
                             , NFData
                             , ToMessage, ToMarkup)
$(deriveLift ''WxppMediaID)

fromWxppBriefMediaID :: WxppBriefMediaID -> WxppMediaID
fromWxppBriefMediaID = WxppMediaID . unWxppBriefMediaID

fromWxppDurableMediaID :: WxppDurableMediaID -> WxppMediaID
fromWxppDurableMediaID = WxppMediaID . unWxppDurableMediaID

instance ToJSON WxppMediaID where
    toJSON = toJSON . unWxppMediaID

instance FromJSON WxppMediaID where
  parseJSON = fmap WxppMediaID
                . (parseJSON >=> nonEmptyJsonText "Weixin media id cannot be empty text")


newtype WxppOpenID = WxppOpenID { unWxppOpenID :: Text}
                    deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
                             , ToHttpApiData, FromHttpApiData
                             , NFData
                             , PersistField, PersistFieldSql
                             , ToJSON
                             , ToMessage, ToMarkup)

-- {{{1 instances
instance SafeCopy WxppOpenID where
    getCopy                 = contain $ WxppOpenID <$> safeGet
    putCopy (WxppOpenID x)  = contain $ safePut x
    errorTypeName _         = "WxppOpenID"

instance FromJSON WxppOpenID where
  parseJSON = fmap WxppOpenID
                . (parseJSON >=> nonEmptyJsonText "Weixin open id cannot be empty text")

instance PathPiece WxppOpenID where
    toPathPiece (WxppOpenID x)  = toPathPiece x
    fromPathPiece t             =   let t' = T.strip t
                                    in if T.null t'
                                          then Nothing
                                          else WxppOpenID <$> fromPathPiece t'
-- }}}1


newtype WxppUnionID = WxppUnionID { unWxppUnionID :: Text }
                    deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
                             , ToHttpApiData, FromHttpApiData
                             , NFData
                             , PersistField, PersistFieldSql
                             , ToJSON
                             , ToMessage, ToMarkup)

-- {{{1 instances
instance FromJSON WxppUnionID where
  parseJSON = fmap WxppUnionID
                . (parseJSON >=> nonEmptyJsonText "Weixin union id cannot be empty text")

instance SafeCopy WxppUnionID where
    getCopy                 = contain $ WxppUnionID <$> safeGet
    putCopy (WxppUnionID x) = contain $ safePut x
    errorTypeName _         = "WxppUnionID"

instance PathPiece WxppUnionID where
    toPathPiece (WxppUnionID x) = toPathPiece x
    fromPathPiece t             = WxppUnionID <$> fromPathPiece t
-- }}}1


newtype WxppInMsgID = WxppInMsgID { unWxppInMsgID :: Word64 }
                    deriving (Show, Eq, Ord, Typeable, Generic, Binary
                             , NFData
                             , PersistField, PersistFieldSql
                             , ToJSON, FromJSON
                             , ToMarkup)


-- | 二维码场景ID
-- 从文档“生成带参数的二维码”一文中看
-- 场景ID可以是个32位整数，也可以是个字串。有若干约束。
newtype WxppIntSceneID = WxppIntSceneID { unWxppIntSceneID :: Word32 }
  deriving (Show, Eq, Ord, Typeable, Generic, Binary, NFData, ToMarkup
           , PersistField, PersistFieldSql
           )

newtype WxppStrSceneID = WxppStrSceneID { unWxppStrSceneID :: Text }
                    deriving (Show, Eq, Ord, Typeable, Generic, Binary
                             , NFData
                             , PersistField, PersistFieldSql
                             , ToMessage, ToMarkup)

data WxppScene =    WxppSceneInt WxppIntSceneID
                    | WxppSceneStr WxppStrSceneID
                    deriving (Show, Eq, Ord, Typeable, Generic)

-- {{{1 instances
instance NFData WxppScene
instance Binary WxppScene

instance ToJSON WxppScene where
    toJSON (WxppSceneInt (WxppIntSceneID x)) = object [ "scene_id" .= x ]
    toJSON (WxppSceneStr (WxppStrSceneID x)) = object [ "scene_str" .= x ]

instance FromJSON WxppScene where
    parseJSON = withObject "WxppScene" $ \obj -> do
        r <- (Left <$> obj .: "scene_id") ClassyPrelude.<|> (Right <$> obj .: "scene_str")
        case r of
            Left i -> return $ WxppSceneInt $ WxppIntSceneID i
            Right t -> do
                when ( T.length t < 1 || T.length t > 64) $ do
                    fail $ "invalid scene id str length"
                return $ WxppSceneStr $ WxppStrSceneID t

-- | 此实例实现对应于 WxppScene 在 XML 的编码方式
-- qrscene 前缀见“接收事件推送”一文
-- 但文档仅在“用户未关注时……”这一情况下说有些前缀
-- 另一种情况“用户已关注……”时则只说是个 32 位整数
-- 因此目前不知道如果创建时用的是字串型场景ID，在后一种情况下会是什么样子
-- 测试结果：qrscene_ 的确是有时有，有时无
instance SimpleEncode WxppScene where
    simpleEncode (WxppSceneInt (WxppIntSceneID x)) = "qrscene_" ++ show x
    simpleEncode (WxppSceneStr (WxppStrSceneID x)) = "qrscene_" ++ T.unpack x

instance SimpleStringRep WxppScene where
    simpleParser = try parse_as_int Text.Parsec.<|> parse_as_str
        where
            parse_as_int = do
                _ <- optional $ string "qrscene_"
                WxppSceneInt . WxppIntSceneID <$> simpleParser

            parse_as_str = do
                _ <- optional $ string "qrscene_"
                WxppSceneStr . WxppStrSceneID . fromString <$> many1 anyChar
-- }}}1


-- | 创建二维码接口的返回报文
data WxppMakeSceneResult = WxppMakeSceneResult
                                QRTicket
                                (Maybe NominalDiffTime)
                                UrlText

-- {{{1 instances
instance FromJSON WxppMakeSceneResult where
    parseJSON = withObject "WxppMakeSceneResult" $ \obj -> do
                    WxppMakeSceneResult
                        <$> ( obj .: "ticket" )
                        <*> ( fmap (fromIntegral :: Int -> NominalDiffTime) <$> obj .:? "expire_seconds")
                        <*> ( UrlText <$> obj .: "url" )

instance ToJSON WxppMakeSceneResult where
    toJSON (WxppMakeSceneResult ticket m_ttl url) =
        object  $ catMaybes $
                [ Just $ "ticket" .= ticket
                , flip fmap m_ttl $ \ttl -> "expire_seconds" .= (round ttl :: Int)
                , Just $ "url" .= unUrlText url
                ]
-- }}}1


newtype QRTicket = QRTicket { unQRTicket :: Text }
                    deriving (Show, Eq, Ord, Typeable, Generic, Binary
                             , NFData
                             , PersistField, PersistFieldSql
                             , ToJSON, FromJSON
                             , ToMessage, ToMarkup)


newtype Token = Token { unToken :: Text }
                    deriving (Show, Eq, Ord, PersistFieldSql, PersistField
                             , NFData
                             , ToMessage, ToMarkup
                             )

instance FromJSON Token where
  parseJSON = fmap Token . (parseJSON >=> nonEmptyJsonText "Weixin Token cannot be empty text")


newtype AesKey = AesKey { unAesKey :: Key AES }
  deriving (Eq)

-- {{{1 instances and functions
instance Show AesKey where
    show (AesKey k) = "AesKey:" <> (C8.unpack $ B64.encode $ toBytes k)

instance PersistField AesKey where
    toPersistValue      = toPersistValue . toBytes . unAesKey
    fromPersistValue    = (fromPersistValue :: PersistValue -> Either Text ByteString)
                            >=> either (Left . fromString . show) (Right . AesKey)
                                . makeKey

instance PersistFieldSql AesKey where
    sqlType _ = sqlType (Proxy :: Proxy ByteString)

instance ToMessage AesKey where
    toMessage (AesKey k) = T.filter (/= '=') $ decodeUtf8 $ B64.encode $ toBytes k

instance ToMarkup AesKey where
    toMarkup = toMarkup . toMessage
    preEscapedToMarkup = preEscapedToMarkup . toMessage

decodeBase64Encoded :: Text -> Either String ByteString
decodeBase64Encoded = B64.decode . encodeUtf8

decodeBase64AesKey :: Text -> Either String AesKey
decodeBase64AesKey t = fmap AesKey $ do
    decodeBase64Encoded t' >>= either (Left . show) Right . makeKey
    where
        -- 相比标准的 base64 微信显示的 AesKey 少了补位的等号
        t' = t <> T.replicate (4 - length t `rem` 4) "="

parseAesKeyFromText :: Text -> Parser AesKey
parseAesKeyFromText t = either fail return $ decodeBase64AesKey t

-- | Like parseAesKeyFromText, but return Nothing if text is empty
parseAesKeyFromTextMaybe :: Text -> Parser (Maybe AesKey)
parseAesKeyFromTextMaybe t =
  let t' = T.strip t
   in if null t'
         then return Nothing
         else fmap Just $ parseAesKeyFromText t'

instance FromJSON AesKey where
    parseJSON = withText "AesKey" parseAesKeyFromText
-- }}}1


newtype TimeStampS = TimeStampS { unTimeStampS :: Text }
  deriving (Show, Eq, NFData)

newtype Nonce = Nonce { unNounce :: Text }
  deriving (Show, Eq, ToMessage, NFData, ToMarkup)


newtype WxppAppID = WxppAppID { unWxppAppID :: Text }
                    deriving (Show, Eq, Ord, Typeable, Generic, Binary
                             , ToHttpApiData, FromHttpApiData
                             , NFData
                             , PersistField, PersistFieldSql
                             , ToJSON
                             , ToMessage, ToMarkup)

-- {{{1 instances and functions

-- | Test if a text can be a app id
-- undocumented rules, just wild guess
validateWxppAppIdText :: Text -> Bool
validateWxppAppIdText t = not (null t) && all (\x -> isAscii x && isAlphaNum x) t

instance SafeCopy WxppAppID where
    getCopy                 = contain $ WxppAppID <$> safeGet
    putCopy (WxppAppID x)   = contain $ safePut x
    errorTypeName _         = "WxppAppID"

instance PathPiece WxppAppID where
    toPathPiece (WxppAppID x)   = toPathPiece x
    fromPathPiece t             =   let t' = T.strip t
                                    in do
                                      guard $ validateWxppAppIdText t'
                                      if T.null t'
                                        then Nothing
                                        else WxppAppID <$> fromPathPiece t'

instance FromJSON WxppAppID where
  parseJSON = fmap WxppAppID
                . (parseJSON >=> nonEmptyJsonText "Weixin app id cannot be empty text")


-- | XXX: Read instance 目前只是 Yesod 生成的 Route 类型时用到
-- 但不清楚具体使用场景，不知道以下的定义是否合适
instance Read WxppAppID where
    readsPrec d s = map (WxppAppID *** id) $ readsPrec d s
-- }}}1


-- | app id 及对应的 open id
data WxppAppOpenID = WxppAppOpenID WxppAppID WxppOpenID
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

-- {{{1 instances
instance NFData WxppAppOpenID

$(deriveSafeCopy 0 'base ''WxppAppOpenID)

instance Binary WxppAppOpenID

instance FromJSON WxppAppOpenID where
  parseJSON = withObject "WxppAppOpenID" $ \o -> do
                WxppAppOpenID <$> o .: "app_id"
                              <*> o .: "open_id"

instance ToJSON WxppAppOpenID where
  toJSON (WxppAppOpenID app_id open_id) =
    object [ "app_id" .= app_id, "open_id" .= open_id ]

instance PathPiece WxppAppOpenID where
  toPathPiece (WxppAppOpenID app_id open_id) =
    mconcat [ unWxppAppID app_id, "@", unWxppOpenID open_id ]

  fromPathPiece t = do
    app_id <- fromPathPiece app_id_t
    open_id <- T.stripPrefix "@" other_t >>= fromPathPiece
    return $ WxppAppOpenID app_id open_id
    where
      (app_id_t, other_t) = T.breakOn "@" t
-- }}}1


-- | 为保证 access_token 的值与它生成属的 app 一致
-- 把它们打包在一个类型里
data AccessToken = AccessToken {
                        accessTokenData     :: Text
                        , accessTokenApp    :: WxppAppID
                    }
                    deriving (Show, Eq, Typeable, Generic)

-- {{{1 instances
instance NFData AccessToken

$(deriveSafeCopy 0 'base ''AccessToken)

instance ToJSON AccessToken where
    toJSON x = object   [ "data"    .= accessTokenData x
                        , "app_id"  .= accessTokenApp x
                        ]

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \obj -> do
                    AccessToken <$> (obj .: "data")
                                <*> (obj .: "app_id")
-- }}}1


-- | 等待额外的值以完整地构造 AccessToken
type AccessTokenP = WxppAppID -> AccessToken

newtype WxppAppSecret = WxppAppSecret { unWxppAppSecret :: Text }
                    deriving (Show, Eq, PersistFieldSql, PersistField
                             , NFData
                             , ToMessage, ToMarkup
                             )

instance ToJSON WxppAppSecret where toJSON = toJSON . unWxppAppSecret

instance FromJSON WxppAppSecret where parseJSON = fmap WxppAppSecret . parseJSON

-- | 这个结构里包含了全部与公众号相关的设置性数据
-- 考虑了第三方平台的需求后，现在对设置性数据作了一点区分：
-- 有部分只是为了跟微信平台通讯的隐私数据：app id, aes key, secret
-- 另一部分是我们特定的代码实现方式而产生的数据：data dir
data WxppAppConf = WxppAppConf
  { wxppConfAppID              :: WxppAppID
  , wxppConfAppSecret        :: WxppAppSecret
  , wxppConfAppToken         :: Token
  , wxppConfAppAesKey        :: Maybe AesKey
  , wxppConfAppBackupAesKeys :: [AesKey]
  -- ^ 多个 aes key 是为了过渡时使用
  -- 加密时仅使用第一个
  -- 解密时则则所有都试一次
  }
  deriving (Show, Eq)

-- {{{1 instances
instance FromJSON WxppAppConf where
    parseJSON = withObject "WxppAppConf" $ \obj -> do
                    app_id <- fmap WxppAppID $ obj .: "app-id"
                    secret <- fmap WxppAppSecret $ obj .: "secret"
                    app_token <- fmap Token $ obj .: "token"
                    aes_key_lst <- obj .: "aes-key"
                                    >>= return . filter (not . T.null) . map T.strip
                                    >>= mapM parseAesKeyFromText
                    let (ak1, backup_aks) =
                                case aes_key_lst of
                                    []      -> (Nothing, [])
                                    (x:xs)  -> (Just x, xs)

                    return $ WxppAppConf app_id secret app_token
                                ak1
                                backup_aks
-- }}}1


data WxppAppConfig = WxppAppConfig
  { wxppConfigCore :: WxppAppConf
  , wxppAppConfigDataDir  :: NonEmpty FilePath
  }
  deriving (Show, Eq)


instance FromJSON WxppAppConfig where
    parseJSON = withObject "WxppAppConfig" $ \obj -> do
                    conf <- parseJSON $ toJSON obj
                    data_dirs <- map T.unpack <$> obj .: "data-dirs"
                    data_dirs' <- case nonEmpty data_dirs of
                                    Nothing -> fail "data-dirs must not be empty"
                                    Just x -> return x


                    return $ WxppAppConfig conf data_dirs'

-- | for backward-compatibility
wxppAppConfigAppID :: WxppAppConfig -> WxppAppID
wxppAppConfigAppID = wxppConfAppID . wxppConfigCore


-- | 见高级群发接口文档
data GroupSendStatus =    GroupSendSuccess
                        | GroupSendFail
                        | GroupSendError Int
                        deriving (Show, Eq, Typeable, Generic)

-- {{{1 instances
instance NFData GroupSendStatus
instance Binary GroupSendStatus

$(deriveJsonS "GroupSendStatus")

instance SimpleEncode GroupSendStatus where
    simpleEncode GroupSendSuccess   = "send success"
    simpleEncode GroupSendFail      = "send fail"
    simpleEncode (GroupSendError x) = "err(" <> show x <> ")"

instance SimpleStringRep GroupSendStatus where
    simpleParser = choice
                    [ try $ string "send success" >> return GroupSendSuccess
                    , try $ string "send fail" >> return GroupSendFail
                    , parse_err
                    ]
        where
            parse_err = do
                _ <- string "err("
                code <- simpleParser
                _ <- string ")"
                return  $ GroupSendError code
-- }}}1


-- | 事件推送的各种值
data WxppEvent = WxppEvtSubscribe
                | WxppEvtUnsubscribe
                | WxppEvtSubscribeAtScene WxppScene (Maybe QRTicket)
                | WxppEvtScan WxppScene (Maybe QRTicket)
                | WxppEvtScanCodePush Text Text Text
                    -- ^ event key, scan type, scan result
                    -- XXX: 文档有提到这个事件类型，但没有消息的具体细节
                | WxppEvtScanCodeWaitMsg Text Text Text
                    -- ^ event key, scan type, scan result
                    -- XXX: 文档有提到这个事件类型，但没有消息的具体细节
                | WxppEvtReportLocation (Double, Double) Double
                    -- ^ (纬度，经度） 精度
                | WxppEvtClickItem Text
                | WxppEvtFollowUrl UrlText
                | WxppEvtGroupSendReport GroupSendStatus Int Int Int Int
                    -- ^ status, total, filter count, sent count, error count
                deriving (Show, Eq, Typeable, Generic)

-- {{{1 instances
instance NFData WxppEvent
instance Binary WxppEvent

wxppEventIsSubscribe :: WxppEvent -> Bool
wxppEventIsSubscribe WxppEvtSubscribe             = True
wxppEventIsSubscribe (WxppEvtSubscribeAtScene {}) = True
wxppEventIsSubscribe _                            = False


wxppEventTypeString :: IsString a => WxppEvent -> a
wxppEventTypeString WxppEvtSubscribe              = "subscribe"
wxppEventTypeString WxppEvtUnsubscribe            = "unsubscribe"
wxppEventTypeString (WxppEvtSubscribeAtScene {})  = "subscribe_at_scene"
wxppEventTypeString (WxppEvtScan {})              = "scan"
wxppEventTypeString (WxppEvtReportLocation {})    = "report_location"
wxppEventTypeString (WxppEvtClickItem {})         = "click_item"
wxppEventTypeString (WxppEvtFollowUrl {})         = "follow_url"
wxppEventTypeString (WxppEvtScanCodePush {})      = "scancode_push"
wxppEventTypeString (WxppEvtScanCodeWaitMsg {})   = "scancode_waitmsg"
wxppEventTypeString (WxppEvtGroupSendReport {})   = "MASSSENDJOBFINISH"

wxppEventTypeStringOfSubs :: IsString a => [a]
wxppEventTypeStringOfSubs = [ "subscribe"
                            , "subscribe_at_scene"
                            ]

wxppEventTypeStringOfUnsubs :: IsString a => [a]
wxppEventTypeStringOfUnsubs = [ "unsubscribe"
                              ]


instance ToJSON WxppEvent where
    toJSON e = object $ ("type" .= (wxppEventTypeString e :: Text)) : get_others e
      where
        get_others WxppEvtSubscribe     = []
        get_others WxppEvtUnsubscribe   = []

        get_others (WxppEvtSubscribeAtScene scene_id qrt) =
                                          [ "scene"     .= scene_id
                                          , "qr_ticket" .= qrt
                                          ]

        get_others (WxppEvtScan scene_id qrt) =
                                          [ "scene"     .= scene_id
                                          , "qr_ticket" .= qrt
                                          ]

        get_others(WxppEvtScanCodePush key scan_type scan_result) =
                                          [ "key"         .= key
                                          , "scan_type"   .= scan_type
                                          , "scan_result" .= scan_result
                                          ]

        get_others(WxppEvtScanCodeWaitMsg key scan_type scan_result) =
                                          [ "key"         .= key
                                          , "scan_type"   .= scan_type
                                          , "scan_result" .= scan_result
                                          ]

        get_others (WxppEvtReportLocation (latitude, longitude) scale) =
                                          [ "latitude"  .= latitude
                                          , "longitude" .= longitude
                                          , "scale"     .= scale
                                          ]

        get_others (WxppEvtClickItem key) = [ "key" .= key ]

        get_others (WxppEvtFollowUrl url) = [ "url" .= unUrlText url ]

        get_others (WxppEvtGroupSendReport status total f_cnt sent_cnt err_cnt) =
                                          [ "status"        .= status
                                          , "total_count"   .= total
                                          , "filter_count"  .= f_cnt
                                          , "sent_count"    .= sent_cnt
                                          , "error_count"   .= err_cnt
                                          ]


instance FromJSON WxppEvent where
    parseJSON = withObject "WxppEvent" $ \obj -> do
        typ <- obj .: "type"
        case typ of
          "subscribe"   -> return WxppEvtSubscribe
          "unsubscribe" -> return WxppEvtUnsubscribe

          "subscribe_at_scene" -> liftM2 WxppEvtSubscribeAtScene
                                      (obj .: "scene")
                                      (obj .:? "qr_ticket")

          "scan"  -> liftM2 WxppEvtScan
                              (obj .: "scene")
                              (obj .:? "qr_ticket")

          "scancode_push" -> WxppEvtScanCodePush <$> obj .: "key"
                                                <*> obj .: "scan_type"
                                                <*> obj .: "scan_result"

          "scancode_waitmsg" -> WxppEvtScanCodeWaitMsg <$> obj .: "key"
                                                        <*> obj .: "scan_type"
                                                        <*> obj .: "scan_result"

          "report_location" -> liftM2 WxppEvtReportLocation
                                  (liftM2 (,) (obj .: "latitude") (obj .: "longitude"))
                                  (obj .: "scale")

          "click_item"  -> WxppEvtClickItem <$> obj .: "key"

          "follow_url"  -> WxppEvtFollowUrl . UrlText <$> obj .: "url"

          "MASSSENDJOBFINISH" -> WxppEvtGroupSendReport
                                    <$> obj .: "status"
                                    <*> obj .: "total_count"
                                    <*> obj .: "filter_count"
                                    <*> obj .: "sent_count"
                                    <*> obj .: "error_count"


          _ -> fail $ "unknown type: " ++ typ
-- }}}1


-- | 收到的各种消息: 包括普通消息和事件推送
data WxppInMsg =  WxppInMsgText Text
                    -- ^ 文本消息
                | WxppInMsgImage WxppBriefMediaID UrlText
                    -- ^ 图片消息 media_id url
                | WxppInMsgVoice WxppBriefMediaID Text (Maybe Text)
                    -- ^ format recognition
                | WxppInMsgVideo WxppBriefMediaID WxppBriefMediaID
                    -- ^ media_id, thumb_media_id
                | WxppInMsgShortVideo WxppBriefMediaID WxppBriefMediaID
                    -- ^ media_id, thumb_media_id
                | WxppInMsgLocation (Double, Double) Double Text
                    -- ^ (latitude, longitude) scale label
                | WxppInMsgLink UrlText Text Text
                    -- ^ url title description
                | WxppInMsgEvent WxppEvent
                deriving (Show, Eq, Typeable, Generic)

-- {{{1 instances and functions
instance NFData WxppInMsg
instance Binary WxppInMsg

wxppInMsgTypeString :: IsString a => WxppInMsg -> a
wxppInMsgTypeString (WxppInMsgText {})      = "text"
wxppInMsgTypeString (WxppInMsgImage {})     = "image"
wxppInMsgTypeString (WxppInMsgVoice {})     = "voice"
wxppInMsgTypeString (WxppInMsgVideo {})     = "video"
wxppInMsgTypeString (WxppInMsgShortVideo {}) = "shortvideo"
wxppInMsgTypeString (WxppInMsgLocation {})  = "location"
wxppInMsgTypeString (WxppInMsgLink {})      = "link"
wxppInMsgTypeString (WxppInMsgEvent {})     = "event"

instance ToJSON WxppInMsg where
    toJSON msg = object $ ("type" .= (wxppInMsgTypeString msg :: Text)) : get_others msg
      where
        get_others (WxppInMsgText t)              = [ "content" .= t ]

        get_others (WxppInMsgImage media_id url)  = [ "media_id"  .= media_id
                                                    , "url"       .= unUrlText url
                                                    ]

        get_others (WxppInMsgVoice media_id format reg) =
                                                    [ "media_id"    .= media_id
                                                    , "format"      .= format
                                                    , "recognition" .= reg
                                                    ]

        get_others (WxppInMsgVideo media_id thumb_media_id) =
                                                    [ "media_id"        .= media_id
                                                    , "thumb_media_id"  .= thumb_media_id
                                                    ]

        get_others (WxppInMsgShortVideo media_id thumb_media_id) =
                                                    [ "media_id"        .= media_id
                                                    , "thumb_media_id"  .= thumb_media_id
                                                    ]

        get_others (WxppInMsgLocation (latitude, longitude) scale loc_label) =
                                                    [ "latitude"  .= latitude
                                                    , "longitude" .= longitude
                                                    , "scale"     .= scale
                                                    , "label"     .= loc_label
                                                    ]

        get_others (WxppInMsgLink url title desc) = [ "url"   .= unUrlText url
                                                    , "title" .= title
                                                    , "desc"  .= desc
                                                    ]

        get_others (WxppInMsgEvent evt) = [ "event" .= evt ]


instance FromJSON WxppInMsg where
    parseJSON = withObject "WxppInMsg" $ \obj -> do
      typ <- obj .: "type"
      case typ of
        "text" -> WxppInMsgText <$> obj .: "content"

        "image" -> liftM2 WxppInMsgImage
                    (obj .: "media_id")
                    (UrlText <$> obj .: "url")

        "voice" -> liftM3 WxppInMsgVoice
                    (obj .: "media_id")
                    (obj .: "format")
                    (join . fmap nullToNothing <$> obj .:? "recognition")

        "video" -> liftM2 WxppInMsgVideo
                    (obj .: "media_id")
                    (obj .: "thumb_media_id")

        "shortvideo" -> liftM2 WxppInMsgShortVideo
                            (obj .: "media_id")
                            (obj .: "thumb_media_id")

        "location" -> liftM3 WxppInMsgLocation
                        (liftM2 (,) (obj .: "latitude") (obj .: "longitude"))
                        (obj .: "scale")
                        (obj .: "label")
        "link"  -> liftM3 WxppInMsgLink
                        (UrlText <$> obj .: "url")
                        (obj .: "title")
                        (obj .: "desc")

        "event" -> WxppInMsgEvent <$> obj .: "event"

        _ -> fail $ "unknown type: " ++ typ
-- }}}1


data WxppInMsgEntity = WxppInMsgEntity
                        {
                            wxppInToUserName        :: WeixinUserName
                            , wxppInFromUserName    :: WxppOpenID
                            , wxppInCreatedTime     :: UTCTime
                            , wxppInMessageID       :: Maybe WxppInMsgID
                                -- ^ 从文档看，除了事件通知，所有信息都有 MsgID
                            , wxppInMessage         :: WxppInMsg
                        }
                        deriving (Show, Eq, Typeable, Generic)

-- {{{1 instances and functions
instance NFData WxppInMsgEntity
instance Binary WxppInMsgEntity

-- | 用于排重的值
type WxppInMsgAmostUniqueID = Either WxppInMsgID (WxppOpenID, UTCTime)

almostUniqueIdOfWxppInMsgEntity :: WxppInMsgEntity -> WxppInMsgAmostUniqueID
almostUniqueIdOfWxppInMsgEntity ime = case wxppInMessageID ime of
                                        Just msg_id -> Left msg_id
                                        Nothing     -> Right $ (wxppInFromUserName &&& wxppInCreatedTime) ime

instance ToJSON WxppInMsgEntity where
    toJSON e = object [ "to"            .= wxppInToUserName e
                      , "from"          .= wxppInFromUserName e
                      , "created_time"  .= wxppInCreatedTime e
                      , "msg_id"        .= wxppInMessageID e
                      , "msg"           .= wxppInMessage e
                      ]

instance FromJSON WxppInMsgEntity where
    parseJSON = withObject "WxppInMsgEntity" $ \obj -> do
                  liftM5 WxppInMsgEntity
                      (obj .: "to")
                      (obj .: "from")
                      (obj .: "created_time")
                      (obj .:? "msg_id")
                      (obj .: "msg")
-- }}}1


-- | 图文信息
data WxppArticle = WxppArticle {
                    wxppArticleTitle    :: Maybe Text    -- ^ title
                    , wxppArticleDesc   :: Maybe Text    -- ^ description
                    , wxppArticlePicUrl :: Maybe UrlText -- ^ pic url
                    , wxppArticleUrl    :: Maybe UrlText -- ^ url
                    }
                    deriving (Show, Eq, Typeable, Generic)

-- {{{1 instances
instance NFData WxppArticle
instance Binary WxppArticle
$(deriveLift ''WxppArticle)

instance ToJSON WxppArticle where
    toJSON wa = object
                  [ "title"   .= wxppArticleTitle wa
                  , "desc"    .= wxppArticleDesc wa
                  , "pic-url" .= (unUrlText <$> wxppArticlePicUrl wa)
                  , "url"     .= (unUrlText <$> wxppArticleUrl wa)
                  ]

type WxppArticleLoader = DelayedYamlLoader WxppArticle

instance FromJSON WxppArticle where
    parseJSON = withObject "WxppArticle" $ \obj -> do
                title <- join . fmap nullToNothing <$> obj .:? "title"
                desc <- join . fmap nullToNothing <$> obj .:? "desc"
                pic_url <- fmap UrlText <$> join . fmap nullToNothing <$> obj .:? "pic-url"
                url <- fmap UrlText <$> join . fmap nullToNothing <$> obj .:? "url"
                return $ WxppArticle title desc pic_url url
-- }}}1


-- | 外发的信息
-- XXX: 虽然文档没有明确说明，media_id是否可以是永久素材的ID，
--      目录假定这是可以的，因为许多其它接口都支付永久素材了
data WxppOutMsg = WxppOutMsgText Text
                | WxppOutMsgImage WxppMediaID
                | WxppOutMsgVoice WxppMediaID
                | WxppOutMsgVideo WxppMediaID (Maybe WxppMediaID) (Maybe Text) (Maybe Text)
                    -- ^ media_id thumb_media_id title description
                    -- XXX: 缩略图字段出现在"客服接口"文档里，
                    -- 但又没出现在回复用户消息文档里
                    -- 暂时为它留着一个字段
                | WxppOutMsgMusic WxppMediaID (Maybe Text) (Maybe Text) (Maybe UrlText) (Maybe UrlText)
                    -- ^ thumb_media_id, title, description, url, hq_url
                | WxppOutMsgNews [WxppArticle]
                    -- ^ 根据文档，图文总数不可超过10
                | WxppOutMsgTransferToCustomerService
                    -- ^ 把信息转发至客服
                deriving (Show, Eq, Typeable, Generic)

-- {{{1 instances and functions
instance NFData WxppOutMsg
instance Binary WxppOutMsg
$(deriveLift ''WxppOutMsg)

wxppOutMsgTypeString :: IsString a => WxppOutMsg -> a
wxppOutMsgTypeString (WxppOutMsgText {})                    = "text"
wxppOutMsgTypeString (WxppOutMsgImage {})                   = "image"
wxppOutMsgTypeString (WxppOutMsgVoice {})                   = "voice"
wxppOutMsgTypeString (WxppOutMsgVideo {})                   = "video"
wxppOutMsgTypeString (WxppOutMsgMusic {})                   = "music"
wxppOutMsgTypeString (WxppOutMsgNews {})                    = "news"
wxppOutMsgTypeString (WxppOutMsgTransferToCustomerService)  = "transfer-cs"

instance ToJSON WxppOutMsg where
    toJSON outmsg = object $ ("type" .= (wxppOutMsgTypeString outmsg :: Text)) : get_others outmsg
      where
        get_others (WxppOutMsgText t) = [ "text" .= t ]
        get_others (WxppOutMsgImage media_id) = [ "media_id" .= media_id ]
        get_others (WxppOutMsgVoice media_id) = [ "media_id" .= media_id ]
        get_others (WxppOutMsgVideo media_id thumb_media_id title desc) =
                                                [ "media_id"        .= media_id
                                                , "thumb_media_id"  .= thumb_media_id
                                                , "title"           .= title
                                                , "desc"            .= desc
                                                ]
        get_others (WxppOutMsgMusic thumb_media_id title desc url hq_url) =
                                                [ "thumb_media_id"  .= thumb_media_id
                                                , "title"           .= title
                                                , "desc"            .= desc
                                                , "url"             .= (unUrlText <$> url)
                                                , "hq_url"          .= (unUrlText <$> hq_url)
                                                ]
        get_others (WxppOutMsgNews articles) = [ "articles" .= articles ]
        get_others (WxppOutMsgTransferToCustomerService) = []


instance FromJSON WxppOutMsg where
    parseJSON = withObject "WxppOutMsg" $ \obj -> do
      typ <- obj .:? "type" .!= "text"
      case (typ :: String) of
          "text"  -> WxppOutMsgText <$> obj .: "text"
          "image" -> WxppOutMsgImage <$> obj .: "media_id"
          "voice" -> WxppOutMsgVoice <$> obj .: "media_id"

          "video" -> liftM4 WxppOutMsgVideo
                        (obj .: "media_id")
                        (obj .: "thumb_media_id")
                        (join . fmap nullToNothing <$> obj .:? "title")
                        (join . fmap nullToNothing <$> obj .:? "desc")

          "music" -> liftM5 WxppOutMsgMusic
                        (obj .: "thumb_media_id")
                        (join . fmap nullToNothing <$> obj .:? "title")
                        (join . fmap nullToNothing <$> obj .:? "desc")
                        (fmap UrlText <$> join . fmap nullToNothing <$> obj .:? "url")
                        (fmap UrlText <$> join . fmap nullToNothing <$> obj .:? "hq_url")

          "news" -> WxppOutMsgNews <$> obj .: "articles"

          "transfer-cs" -> return WxppOutMsgTransferToCustomerService

          _   -> fail $ "unknown type: " ++ typ
-- }}}1


type WxppMediaIdOrPath = Either WxppMediaID FilePath

-- | 外发的信息的本地信息
-- 因 WxppOutMsg 包含 media id，它只在上传3天内有效，这个类型的值代表的就是相应的本地长期有效的信息
data WxppOutMsgL = WxppOutMsgTextL Text
                | WxppOutMsgImageL WxppMediaIdOrPath
                | WxppOutMsgVoiceL WxppMediaIdOrPath
                | WxppOutMsgVideoL WxppMediaIdOrPath (Maybe WxppMediaIdOrPath) (Maybe Text) (Maybe Text)
                    -- ^ media_id thumb_image title description
                    -- XXX: 缩略图字段出现在"客服接口"文档里，
                    -- 但又没出现在回复用户消息文档里
                    -- 暂时为它留着一个字段
                | WxppOutMsgMusicL WxppMediaIdOrPath (Maybe Text) (Maybe Text) (Maybe UrlText) (Maybe UrlText)
                    -- ^ thumb_media_id, title, description, url, hq_url
                | WxppOutMsgNewsL [WxppArticleLoader]
                    -- ^ 根据文档，图文总数不可超过10
                | WxppOutMsgTransferToCustomerServiceL
                    -- ^ 把信息转发至客服

type WxppOutMsgLoader = DelayedYamlLoader WxppOutMsgL

parseMediaIDOrPath :: Text -> Object -> Parser WxppMediaIdOrPath
parseMediaIDOrPath key_prefix o = (Left . WxppMediaID <$> o .: (key_prefix <> "media_id"))
                            ClassyPrelude.<|> (Right <$> o .: (key_prefix <> "path"))

parseMediaIDOrPathOpt :: Text -> Object -> Parser (Maybe WxppMediaIdOrPath)
parseMediaIDOrPathOpt key_prefix o = (fmap (Left . WxppMediaID) <$> o .:? (key_prefix <> "media_id"))
                            ClassyPrelude.<|> (fmap Right <$> o .:? (key_prefix <> "path"))

-- {{{1 instances for WxppOutMsgL
instance FromJSON WxppOutMsgL where
    parseJSON = withObject "WxppOutMsgL" $ \obj -> do
                    type_s <- obj .:? "type" .!= "text"
                    case type_s of
                        "text" -> WxppOutMsgTextL <$> obj .: "text"
                        "image" -> WxppOutMsgImageL <$> parseMediaIDOrPath "" obj
                        "voice" -> WxppOutMsgVoiceL <$> parseMediaIDOrPath "" obj
                        "video" -> do
                            media_id_or_path <- parseMediaIDOrPath "" obj
                            thumb_media_id_or_path <- parseMediaIDOrPathOpt "thumb_" obj
                            title <- obj .:? "title"
                            desc <- obj .:? "desc"
                            return $ WxppOutMsgVideoL media_id_or_path thumb_media_id_or_path title desc

                        "music" -> do
                                    thumb_media_id_or_path <- parseMediaIDOrPath "thumb_" obj
                                    title <- obj .:? "title"
                                    desc <- obj .:? "desc"
                                    url <- fmap UrlText <$> join . fmap nullToNothing <$>
                                                obj .:? "url"
                                    hq_url <- fmap UrlText <$> join . fmap nullToNothing <$>
                                                obj .:? "hq-url"
                                    return $ WxppOutMsgMusicL thumb_media_id_or_path title desc url hq_url

                        "news" -> WxppOutMsgNewsL <$>
                                      ( obj .: "articles"
                                        >>= parseArray "[WxppArticleLoader]" parse_article)

                        "transfer-cs" -> return WxppOutMsgTransferToCustomerServiceL
                        _       -> fail $ "unknown type: " <> type_s
        where

          parse_article_obj = parseDelayedYamlLoader Nothing "file"

          parse_article (A.String t)  = parse_article_obj $ HM.fromList [ "file" .= t ]
          parse_article v             = withObject "WxppArticleLoader" parse_article_obj v
-- }}}1


-- | 永久图文素材结构中的一个文章
data WxppDurableArticle = WxppDurableArticle {
                                wxppDurableArticleTitle            :: Text
                                , wxppDurableArticleThumb          :: WxppDurableMediaID
                                , wxppDurableArticleAuthor         :: Maybe Text
                                , wxppDurableArticleDigest         :: Maybe Text
                                , wxppDurableArticleShowCoverPic   :: Bool
                                , wxppDurableArticleContent        :: Text
                                , wxppDurableArticleContentSrcUrl  :: Maybe UrlText
                            }
                            deriving (Eq, Ord, Show, Generic)

-- {{{1 instances for WxppDurableArticle
instance NFData WxppDurableArticle
$(deriveSafeCopy 0 'base ''WxppDurableArticle)

instance FromJSON WxppDurableArticle where
    parseJSON = withObject "WxppDurableArticle" $ \obj -> do
                    WxppDurableArticle
                        <$> ( obj .: "title" )
                        <*> ( obj .: "thumb_media_id" )
                        <*> ( join . fmap nullToNothing <$> obj .:? "author" )
                        <*> ( join . fmap nullToNothing <$> obj .:? "digest" )
                        <*> ( fmap int_to_bool $ obj .: "show_cover_pic"
                                                    >>= parseIntWithTextparsec natural )
                        <*> ( obj .: "content" )
                        <*> ( fmap UrlText . join . fmap nullToNothing <$> obj .: "content_source_url" )
            where
                int_to_bool x = x /= 0


instance ToJSON WxppDurableArticle where
    toJSON = object . wppDurableArticleToJsonPairs
-- }}}1


wppDurableArticleToJsonPairs :: WxppDurableArticle -> [Pair]
wppDurableArticleToJsonPairs x =
                        [ "title"           .= wxppDurableArticleTitle x
                        , "thumb_media_id"  .= wxppDurableArticleThumb x
                        , "author"          .= (fromMaybe "" $ wxppDurableArticleAuthor x)
                        , "digest"          .= (fromMaybe "" $ wxppDurableArticleDigest x)
                        , "show_cover_pic"  .= (show $ bool_to_int $ wxppDurableArticleShowCoverPic x)
                        , "content"         .= wxppDurableArticleContent x
                        , "content_source_url" .= (fromMaybe "" $ fmap unUrlText $ wxppDurableArticleContentSrcUrl x)
                        ]
            where
                bool_to_int b = if b then 1 :: Int else 0


-- | 永久图文素材结构
newtype WxppDurableNews = WxppDurableNews [WxppDurableArticle]
  deriving (Eq, Ord, NFData)

instance FromJSON WxppDurableNews where
    parseJSON = withObject "WxppDurableNews" $ \obj -> do
                    WxppDurableNews <$> obj .: "articles"

instance ToJSON WxppDurableNews where
    toJSON (WxppDurableNews articles) = object [ "articles" .= articles ]


-- | 上传多媒体文件接口中的 type 参数
data WxppMediaType = WxppMediaTypeImage
                    | WxppMediaTypeVoice
                    | WxppMediaTypeVideo
                    | WxppMediaTypeThumb
                    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- {{{1 instances
instance NFData WxppMediaType
deriveSafeCopy 0 'base ''WxppMediaType

$(derivePersistFieldS "WxppMediaType")
$(derivePathPieceS "WxppMediaType")
$(deriveJsonS "WxppMediaType")
$(deriveSimpleStringRepEnumBounded "WxppMediaType")

instance SimpleEncode WxppMediaType where
    simpleEncode mtype =
        case mtype of
            WxppMediaTypeImage -> "image"
            WxppMediaTypeVoice -> "voice"
            WxppMediaTypeVideo -> "video"
            WxppMediaTypeThumb -> "thumb"
-- }}}1


data WxppOutMsgEntity = WxppOutMsgEntity
                        {
                            wxppOutToUserName       :: WxppOpenID
                            , wxppOutFromUserName   :: WeixinUserName
                            , wxppOutCreatedTime    :: UTCTime
                            , wxppOutMessage        :: WxppOutMsg
                        }
                        deriving (Show, Eq, Generic)

instance NFData WxppOutMsgEntity

wxppMediaTypeString :: IsString a => WxppMediaType -> a
wxppMediaTypeString mtype = fromString $ simpleEncode mtype


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
                    deriving (Show, Eq, Generic)

-- {{{1 instances and functions
instance NFData MenuItemData

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
-- }}}1


-- | 菜单项，及其子菜单
data MenuItem = MenuItem {
                    menuItemName             :: Text
                    , menuItemDataOrSubs    :: Either MenuItemData [MenuItem]
                }
                deriving (Show, Eq)

-- {{{1 instances
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
-- }}}1


newtype SimpleLocaleName = SimpleLocaleName { unSimpleLocaleName :: Text }
  deriving (Show, Eq, Ord, Generic, NFData
           , PersistField, PersistFieldSql
           )

instance SafeCopy SimpleLocaleName where
    getCopy                         = contain $ SimpleLocaleName <$> safeGet
    putCopy (SimpleLocaleName x)    = contain $ safePut x
    errorTypeName _                 = "SimpleLocaleName"


type NickName = Text
type CityName = Text
type ProvinceName = Text
type CountryName = Text

-- | 用户基础信息查询接口的返回
data EndUserQueryResult = EndUserQueryResultNotSubscribed WxppOpenID
                        | EndUserQueryResult
                            WxppOpenID
                            NickName    -- nickname
                            (Maybe Gender)
                            SimpleLocaleName
                            CityName
                            ProvinceName
                            CountryName
                            (Maybe UrlText)     -- head image url
                            UTCTime
                            (Maybe WxppUnionID)
                        deriving (Show, Eq, Ord, Typeable, Generic)

instance NFData EndUserQueryResult

$(deriveSafeCopy 0 'base ''EndUserQueryResult)

endUserQueryResultOpenID :: EndUserQueryResult -> WxppOpenID
endUserQueryResultOpenID (EndUserQueryResultNotSubscribed open_id)      = open_id
endUserQueryResultOpenID (EndUserQueryResult open_id _ _ _ _ _ _ _ _ _) = open_id

endUserQueryResultUnionID :: EndUserQueryResult -> Maybe WxppUnionID
endUserQueryResultUnionID (EndUserQueryResultNotSubscribed {})          = Nothing
endUserQueryResultUnionID (EndUserQueryResult _ _ _ _ _ _ _ _ _ m_uid)  = m_uid

endUserQueryResultSetUnionID :: Maybe WxppUnionID -> EndUserQueryResult -> EndUserQueryResult
endUserQueryResultSetUnionID _      x@(EndUserQueryResultNotSubscribed {})     = x
endUserQueryResultSetUnionID m_uid (EndUserQueryResult x1 x2 x3 x4 x5 x6 x7 x8 x9 _)  =
                                    EndUserQueryResult x1 x2 x3 x4 x5 x6 x7 x8 x9 m_uid

endUserQueryResultSubsTime :: EndUserQueryResult -> Maybe UTCTime
endUserQueryResultSubsTime (EndUserQueryResultNotSubscribed {})             = Nothing
endUserQueryResultSubsTime (EndUserQueryResult _ _ _ _ _ _ _ _ subs_time _) = Just subs_time

endUserQueryResultNickname :: EndUserQueryResult -> Maybe NickName
endUserQueryResultNickname (EndUserQueryResultNotSubscribed {})     = Nothing
endUserQueryResultNickname (EndUserQueryResult _ x _ _ _ _ _ _ _ _) = Just x


endUserQueryResultHeadImgUrl :: EndUserQueryResult -> Maybe (Maybe UrlText)
endUserQueryResultHeadImgUrl (EndUserQueryResultNotSubscribed {})     = Nothing
endUserQueryResultHeadImgUrl (EndUserQueryResult _ _ _ _ _ _ _ x _ _) = Just x


instance FromJSON EndUserQueryResult where
    parseJSON = withObject "EndUserQueryResult" $ \obj -> do
                open_id <- obj .: "openid"
                if_sub <- (\x -> x > (0 :: Int)) <$> obj .: "subscribe"
                if not if_sub
                    then return $ EndUserQueryResultNotSubscribed open_id
                    else do
                        nickname <- obj .: "nickname"
                        m_gender <- obj .: "sex" >>= parseSexJson
                        city <- obj .: "city"
                        lang <- SimpleLocaleName <$> obj .: "language"
                        province <- obj .: "province"
                        country <- obj .: "country"
                        headimgurl <- (fmap UrlText . join . fmap nullToNothing) <$> obj .:? "headimgurl"
                        subs_time <- epochIntToUtcTime <$> obj .: "subscribe_time"
                        m_union_id <- obj .:? "unionid"
                        return $ EndUserQueryResult
                                    open_id
                                    nickname
                                    m_gender
                                    lang
                                    city
                                    province
                                    country
                                    headimgurl
                                    subs_time
                                    m_union_id

instance ToJSON EndUserQueryResult where
    toJSON (EndUserQueryResultNotSubscribed open_id) = object $
        [ "openid"      .= open_id
        , "subscribe"   .= (0 :: Int)
        ]

    toJSON (EndUserQueryResult open_id nickname m_gender lang city province country headimgurl subs_time m_union_id) = object
        [ "openid"      .= open_id
        , "subscribe"   .= (1 :: Int)
        , "nickname"    .= nickname
        , "sex"         .= genderToInt m_gender
        , "language"    .= unSimpleLocaleName lang
        , "city"        .= city
        , "province"    .= province
        , "country"     .= country
        , "headimgurl"  .= headimgurl
        , "subscribe_time".= utcTimeToEpochInt subs_time
        , "unionid"     .= m_union_id
        ]

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

genderToInt :: Maybe Gender -> Int
genderToInt Nothing         = 0
genderToInt (Just Male)     = 1
genderToInt (Just Female)   = 2

-- | 上传媒体文件的结果
data UploadResult = UploadResult {
                        urMediaType     :: WxppMediaType
                        , urMediaId     :: WxppBriefMediaID
                        , urCreateTime  :: UTCTime
                        }
                        deriving (Show, Typeable, Generic)

-- {{{1 instances
instance NFData UploadResult

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
        media_id <- WxppBriefMediaID <$> obj .: "media_id"
        t <- epochIntToUtcTime <$> obj .: "created_at"
        return $ UploadResult typ media_id t
-- }}}1


-- | 转发各种消息或消息的部分时，所附带的额外信息
data WxppForwardedEnv = WxppForwardedEnv {
                            wxppFwdUserInfo         :: EndUserQueryResult
                            , wxppFwdAccessToken    :: AccessToken
                        }

-- {{{1 instances
instance ToJSON WxppForwardedEnv where
    toJSON x = object
                [ "access_token"    .= wxppFwdAccessToken x
                , "user_info"       .= wxppFwdUserInfo x
                ]

instance FromJSON WxppForwardedEnv where
    parseJSON = withObject "WxppForwardedEnv" $ \obj -> do
                    WxppForwardedEnv    <$> ( obj .: "user_info" )
                                        <*> ( obj .: "access_token" )
-- }}}1


data OAuthScope = AS_SnsApiBase
                | AS_SnsApiUserInfo
                | AS_SnsApiLogin
                | AS_Unknown Text
                -- 注意：值的顺序尽量以从低到高排列，以便其它代码自动选择一个合适的值
                deriving (Show, Eq, Ord, Generic)

-- {{{1 instances
instance NFData OAuthScope
$(derivePersistFieldS "OAuthScope")
$(derivePathPieceS "OAuthScope")
$(deriveSafeCopy 0 'base ''OAuthScope)

instance SimpleEncode OAuthScope where
    -- Encode values will be used in wxppAuthPageUrl
    -- so they must be consistent with WX doc.
    simpleEncode AS_SnsApiBase      = "snsapi_base"
    simpleEncode AS_SnsApiUserInfo  = "snsapi_userinfo"
    simpleEncode AS_SnsApiLogin     = "snsapi_login"
    simpleEncode (AS_Unknown s)     = T.unpack s

instance SimpleStringRep OAuthScope where
    simpleParser = try p Text.Parsec.<|> parse_unknown
        where
            p = makeSimpleParserByTable
                    [ ("snsapi_base", AS_SnsApiBase)
                    , ("snsapi_userinfo", AS_SnsApiUserInfo)
                    , ("snsapi_login", AS_SnsApiLogin)
                    ]

            parse_unknown = fmap (AS_Unknown . fromString) $
                                many1 $ satisfy $ not . isSpace
-- }}}1


newtype OAuthCode = OAuthCode { unOAuthCode :: Text }
  deriving (Eq, Ord, Show, PersistField, PersistFieldSql, NFData
           , PathPiece, ToJSON
           , Hashable, Binary
           )


newtype OAuthAccessToken = OAuthAccessToken { unOAuthAccessToken :: Text }
  deriving (Eq, Ord, Show, PersistField, PersistFieldSql, NFData
           , FromJSON, PathPiece
           )

instance SafeCopy OAuthAccessToken where
    getCopy                      = contain $ OAuthAccessToken <$> safeGet
    putCopy (OAuthAccessToken x) = contain $ safePut x
    errorTypeName _              = "OAuthAccessToken"



newtype OAuthRefreshToken = OAuthRefreshToken { unOAuthRefreshToken :: Text }
  deriving (Eq, Ord, Show, PersistField, PersistFieldSql, NFData
           , FromJSON, ToJSON, PathPiece
           )

instance SafeCopy OAuthRefreshToken where
    getCopy                       = contain $ OAuthRefreshToken <$> safeGet
    putCopy (OAuthRefreshToken x) = contain $ safePut x
    errorTypeName _               = "OAuthRefreshToken"


-- | access token 通常要与 open id 一起使用，并且有对应关系，因此打包在一起
data OAuthAccessTokenPkg = OAuthAccessTokenPkg {
                            oauthAtkPRaw        :: OAuthAccessToken
                            , oauthAtkPRtk      :: OAuthRefreshToken
                            , oauthAtkPScopes   :: Set OAuthScope
                            , oauthAtkPOpenID   :: WxppOpenID
                            , oauthAtkPAppID    :: WxppAppID
                            }
                            deriving (Eq, Ord, Show, Generic)

instance NFData OAuthAccessTokenPkg

$(deriveSafeCopy 0 'base ''OAuthAccessTokenPkg)


class HasOAuthAccessTokenPkg a where
  getOAuthAccessTokenPkg :: a -> OAuthAccessTokenPkg

instance HasOAuthAccessTokenPkg OAuthAccessTokenPkg where
  getOAuthAccessTokenPkg = id


data OAuthTokenInfo = OAuthTokenInfo
                        !OAuthAccessToken
                        !OAuthRefreshToken
                        !(Set OAuthScope)
                        !UTCTime
                        deriving (Show, Typeable, Eq, Ord, Generic)

instance NFData OAuthTokenInfo
$(deriveSafeCopy 0 'base ''OAuthTokenInfo)

packOAuthTokenInfo :: WxppAppID
                    -> WxppOpenID
                    -> OAuthTokenInfo
                    -> OAuthAccessTokenPkg
packOAuthTokenInfo app_id open_id (OAuthTokenInfo atk rtk scopes _expiry) =
    OAuthAccessTokenPkg atk rtk scopes open_id app_id


instance HasOAuthAccessTokenPkg ((WxppAppID, WxppOpenID), OAuthTokenInfo) where
  getOAuthAccessTokenPkg ((x, y), z) = packOAuthTokenInfo x y z


fromOAuthAccessTokenResult :: UTCTime
                           -> OAuthAccessTokenResult
                           -> OAuthTokenInfo
fromOAuthAccessTokenResult now r = OAuthTokenInfo
                                    (oauthAtkToken r)
                                    (oauthAtkRefreshToken r)
                                    (oauthAtkScopes r)
                                    (addUTCTime (oauthAtkTTL r) now)


data OAuthAccessTokenResult = OAuthAccessTokenResult {
                                oauthAtkToken           :: OAuthAccessToken
                                , oauthAtkTTL           :: NominalDiffTime
                                , oauthAtkScopes        :: Set OAuthScope
                                , oauthAtkRefreshToken  :: OAuthRefreshToken
                                , oauthAtkOpenID        :: WxppOpenID
                                , oauthAtkUnionID       :: Maybe WxppUnionID
                                }
                                deriving (Eq, Show)

-- {{{1 instances
instance HasOAuthAccessTokenPkg (WxppAppID, OAuthAccessTokenResult) where
  getOAuthAccessTokenPkg (app_id, x) =
      OAuthAccessTokenPkg
        (oauthAtkToken x)
        (oauthAtkRefreshToken x)
        (oauthAtkScopes x)
        (oauthAtkOpenID x)
        app_id


instance FromJSON OAuthAccessTokenResult where
    parseJSON = withObject "OAuthAccessTokenResult" $ \o -> do
                    OAuthAccessTokenResult
                        <$> o .: "access_token"
                        <*> ((fromIntegral :: Int -> NominalDiffTime) <$> o .: "expires_in")
                        <*> (fmap Set.fromList $ o .: "scope"
                              >>= parseTextByParsec wxppParseOAuthScopesList)
                        <*> o .: "refresh_token"
                        <*> o .: "openid"
                        <*> (fmap WxppUnionID . join . fmap nullToNothing <$> o .:? "unionid")

wxppParseOAuthScopesList :: Stream s m Char => ParsecT s u m [OAuthScope]
wxppParseOAuthScopesList = simpleParser `sepBy1` (spaces *> char ',' <* spaces)
-- }}}1


data OAuthRefreshAccessTokenResult = OAuthRefreshAccessTokenResult {
                                        oauthRtkToken           :: OAuthAccessToken
                                        , oauthRtkTTL           :: NominalDiffTime
                                        , oauthRtkScopes        :: Set OAuthScope
                                        , ouahtRtkRefreshToken  :: OAuthRefreshToken
                                        , oauthRtkOpenID        :: WxppOpenID
                                        }
                                        deriving (Eq, Show)

-- {{{1 instances
instance FromJSON OAuthRefreshAccessTokenResult where
    parseJSON = withObject "OAuthRefreshAccessTokenResult" $ \o -> do
                    OAuthRefreshAccessTokenResult
                        <$> o .: "access_token"
                        <*> ((fromIntegral :: Int -> NominalDiffTime) <$> o .: "expires_in")
                        <*> (fmap Set.fromList $ o .: "scope" >>= parseTextByParsec p_scopes)
                        <*> o .: "refresh_token"
                        <*> o .: "openid"
                where
                    p_scopes = simpleParser `sepBy1` (spaces *> char ',' <* spaces)
-- }}}1


data OAuthGetUserInfoResult = OAuthGetUserInfoResult {
                                oauthUserInfoOpenID         :: WxppOpenID
                                , oauthUserInfoNickname     :: Text
                                , oauthUserInfoGender       :: Maybe Gender
                                , oauthUserInfoCountry      :: Text
                                , oauthUserInfoProvince     :: Text
                                , oauthUserInfoCity         :: Text
                                , oauthUserInfoHeadImgUrl   :: Maybe UrlText
                                , oauthUserInfoPrivileges   :: [Text]
                                , oauthUserInfoUnionID      :: Maybe WxppUnionID
                                }
                                deriving (Eq, Show, Typeable)
-- {{{1
$(deriveSafeCopy 0 'base ''OAuthGetUserInfoResult)

instance FromJSON OAuthGetUserInfoResult where
    parseJSON = withObject "OAuthGetUserInfoResult" $ \o -> do
                    OAuthGetUserInfoResult
                        <$> o .: "openid"
                        <*> o .: "nickname"
                        <*> (o .: "sex" >>= parseSexJson)
                        <*> o .: "country"
                        <*> o .: "province"
                        <*> o .: "city"
                        <*> (fmap UrlText . join . fmap nullToNothing <$> o .:? "headimgurl")
                        <*> o .: "privilege"
                        <*> (fmap WxppUnionID . join . fmap nullToNothing <$> o .:? "unionid")
-- }}}1


newtype WxppJsTicket = WxppJsTicket { unWxppJsTicket :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, NFData
           , PersistField, PersistFieldSql
           , FromJSON, ToJSON
           )

-- {{{1
instance SafeCopy WxppJsTicket where
    getCopy                  = contain $ WxppJsTicket <$> safeGet
    putCopy (WxppJsTicket x) = contain $ safePut x
    errorTypeName _          = "WxppJsTicket"

instance PathPiece WxppJsTicket where
    toPathPiece (WxppJsTicket x)  = toPathPiece x
    fromPathPiece t             =   let t' = T.strip t
                                    in if T.null t'
                                          then Nothing
                                          else WxppJsTicket <$> fromPathPiece t'
-- }}}1


-- | 程序内部因公众号的变化而产生的事件
data WxppSignal = WxppSignalNewApp WxppAppID
                | WxppSignalRemoveApp WxppAppID
                deriving (Eq, Show)


-- | 分别描述是否第三方平台时，我们在处理消息时可知道的app id
data ProcAppIdInfo = ProcAppSingle WxppAppID -- ^ 开发者服务器: 只有一个 app id
                    | ProcAppThirdParty WxppAppID WxppAppID
                    -- ^ work as third-party: component_appid, authorizer_appid
                    deriving (Show, Eq, Ord)

procAppIdInfoMyId :: ProcAppIdInfo -> WxppAppID
procAppIdInfoMyId (ProcAppSingle x)       = x
procAppIdInfoMyId (ProcAppThirdParty x _) = x

procAppIdInfoReceiverId :: ProcAppIdInfo -> WxppAppID
procAppIdInfoReceiverId (ProcAppSingle x)       = x
procAppIdInfoReceiverId (ProcAppThirdParty _ x) = x


-- | WxppAppID 所对应的概念现在已包括
-- 微信订阅号，服务号，WEB应用
data WxAppKind = WxAppKindPublisher   -- ^ 订阅号
               | WxAppKindServer      -- ^ 服务号
               | WxAppKindWeb         -- ^ PC网页应用
               | WxAppKindThirdParty  -- ^ 第三方平台
               deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- {{{1 instances
instance NFData WxAppKind

$(derivePersistFieldS "WxAppKind")
$(derivePathPieceS "WxAppKind")
$(deriveJsonS "WxAppKind")
$(deriveSimpleStringRepEnumBounded "WxAppKind")

instance SimpleEncode WxAppKind where
  simpleEncode WxAppKindPublisher  = "publisher"
  simpleEncode WxAppKindServer     = "server"
  simpleEncode WxAppKindWeb        = "web"
  simpleEncode WxAppKindThirdParty = "third-party"
-- }}}1

-- | 哪种app可以做oauth接口调用
-- 注意: 第三方平台只是代替另一个app操作，自身不算可以做oauth
wxAppKindCanOAuth :: WxAppKind -> Bool
wxAppKindCanOAuth WxAppKindServer = True
wxAppKindCanOAuth WxAppKindWeb    = True
wxAppKindCanOAuth _               = False

-- | 哪种app可以关注
wxAppKindCanSubscribe :: WxAppKind -> Bool
wxAppKindCanSubscribe WxAppKindPublisher = True
wxAppKindCanSubscribe WxAppKindServer    = True
wxAppKindCanSubscribe _                  = False


--------------------------------------------------------------------------------

wxppLogSource :: IsString a => a
wxppLogSource = "WXPP"

-- | 上传得到的 media id 只能用一段时间
usableUploadResult :: UTCTime -> NominalDiffTime -> UploadResult -> Bool
usableUploadResult now dt ur = addUTCTime dt (urCreateTime ur) > now


-- vim: set foldmethod=marker:
