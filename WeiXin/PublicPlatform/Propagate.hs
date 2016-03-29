{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Propagate
    ( wxppPropagateUploadNews
    , WxppBriefArticle(..)
    , WxppBriefNews(..)
    , PropagateMsgID(..)
    , WxppPropagateMsg(..)
    , WxppPropagateVideoMediaID(..)
    , wxppPropagateNewVideoID
    , wxppPropagateMsg
    , wxppPreviewPropagateMsg
    , wxppDropPropagateMsg
    , PropagateFilter(..)
    , wxppGetPropagateMsgStatus
    , PropagateMsgStatus(..)
    ) where

import ClassyPrelude
-- import qualified Data.Text.Lazy             as LT
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Control.Monad.Reader                 (asks)
import Data.Aeson
import Data.Aeson.Types                     (Pair)
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..)
                                            , SqlType(..))

import Yesod.Helpers.Utils                  (emptyTextToNothing)
import Yesod.Helpers.Aeson                  (parseIntWithTextparsec)
import Yesod.Helpers.Parsec                 (natural)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils


-- | 准备群发的图文素材结构中的一个文章
-- 从接口的URL及网上文章的讨论看，所谓群发上传图文消息接口估计
-- 本来是“上传临时图文素材”（注意url的media单词）
-- 所上传的每个article内容与 WxppDurableArticle 实质上是一样的
-- 只有一个区别：封面图片必须是临时素材media_id
-- （有传闻说永久素材与临时素材id不能混用）
data WxppBriefArticle = WxppBriefArticle {
                                wxppBriefArticleTitle           :: Text
                                , wxppBriefArticleThumb         :: WxppBriefMediaID
                                , wxppBriefArticleAuthor        :: Maybe Text
                                , wxppBriefArticleDigest        :: Maybe Text
                                , wxppBriefArticleShowCoverPic  :: Bool
                                , wxppBriefArticleContent       :: Text
                                , wxppBriefArticleContentSrcUrl :: Maybe UrlText
                            }
                            deriving (Eq, Show)


instance FromJSON WxppBriefArticle where
    parseJSON = withObject "WxppBriefArticle" $ \obj -> do
                    WxppBriefArticle
                        <$> ( obj .: "title" )
                        <*> ( obj .: "thumb_media_id" )
                        <*> ( join . fmap emptyTextToNothing <$> obj .:? "author" )
                        <*> ( join . fmap emptyTextToNothing <$> obj .:? "digest" )
                        <*> ( fmap int_to_bool $ obj .: "show_cover_pic"
                                                    >>= parseIntWithTextparsec natural )
                        <*> ( obj .: "content" )
                        <*> ( fmap UrlText . join . fmap emptyTextToNothing <$>
                                obj .:? "content_source_url" )
            where
                int_to_bool x = x /= 0


instance ToJSON WxppBriefArticle where
    toJSON x = object   [ "title"           .= wxppBriefArticleTitle x
                        , "thumb_media_id"  .= wxppBriefArticleThumb x
                        , "author"          .= (fromMaybe "" $ wxppBriefArticleAuthor x)
                        , "digest"          .= (fromMaybe "" $ wxppBriefArticleDigest x)
                        , "show_cover_pic"  .= (show $ bool_to_int $ wxppBriefArticleShowCoverPic x)
                        , "content"         .= wxppBriefArticleContent x
                        , "content_source_url" .= (fromMaybe "" $
                                                    unUrlText <$> wxppBriefArticleContentSrcUrl x)
                        ]
            where
                bool_to_int b = if b then 1 :: Int else 0


-- | 可以群发的图文消息
-- 见前面的注释：这个很可能其实是“临时图文素材”
newtype WxppBriefNews = WxppBriefNews [WxppBriefArticle]
                        deriving (Eq, Show)

instance FromJSON WxppBriefNews where
    parseJSON = withObject "WxppBriefNews" $ \obj -> do
                    WxppBriefNews <$> obj .: "articles"

instance ToJSON WxppBriefNews where
    toJSON (WxppBriefNews articles) = object [ "articles" .= articles ]


data PUploadNewsResult = PUploadNewsResult
                            Text
                            WxppBriefMediaID
                            UTCTime

instance FromJSON PUploadNewsResult where
    parseJSON = withObject "PUploadNewsResult" $ \o -> do
                    PUploadNewsResult   <$> o .: "type"
                                        <*> o .: "media_id"
                                        <*> (fmap epochIntToUtcTime $ o .: "created_at")

-- | 群发接口中的 video 的 media id
-- 虽然文档叫这 media id，但又跟其它地方用的 media_id 不一样
-- 因此这里为它定义一个新的类型
newtype WxppPropagateVideoMediaID = WxppPropagateVideoMediaID { unWxppPropagateVideoMediaID :: Text }
                    deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql)

-- | 可以直接群发的消息
data WxppPropagateMsg = WxppPropagateMsgNews WxppMediaID
                            -- ^ 按文档，这个 media_id 须使用 wxppPropagateUploadNews 取得
                            -- 或使用永久素材的 media_id
                        | WxppPropagateMsgText Text
                        | WxppPropagateMsgVoice WxppMediaID
                        | WxppPropagateMsgImage WxppMediaID
                        | WxppPropagateMsgVideo WxppPropagateVideoMediaID
                        | WxppPropagateMsgCard WxCardID
                        deriving (Eq)

instance FromJSON WxppPropagateMsg where
    parseJSON = withObject "WxppPropagateMsg" $ \o -> do
                    typ <- o .: "type"
                    case typ of
                        "mpnews" -> WxppPropagateMsgNews <$> o .: "media_id"
                        "text"  -> WxppPropagateMsgText <$> o .: "content"
                        "voice" -> WxppPropagateMsgVoice <$> o .: "media_id"
                        "image" -> WxppPropagateMsgImage <$> o .: "media_id"
                        "mpvideo" -> WxppPropagateMsgVideo <$> o .: "media_id"
                        "wxcard" -> WxppPropagateMsgCard <$> o .: "card_id"
                        _ -> fail $ "unknown type in WxppPropagateMsg: " <> typ


instance ToJSON WxppPropagateMsg where
    toJSON x = object $ ("type" .= wxppPropagateMsgTypeS x) : wxppPropagateMsgJsonData x


wxppPropagateMsgJsonData :: WxppPropagateMsg -> [ Pair ]
wxppPropagateMsgJsonData x =
    case x of
        WxppPropagateMsgNews media_id   -> [ "media_id" .= media_id ]
        WxppPropagateMsgText t          -> [ "content"  .= t ]
        WxppPropagateMsgVoice media_id  -> [ "media_id" .= media_id ]
        WxppPropagateMsgImage media_id  -> [ "media_id" .= media_id ]
        WxppPropagateMsgVideo media_id  -> [ "media_id" .= media_id ]
        WxppPropagateMsgCard card_id    -> [  "card_id" .= card_id ]

-- | 这个字段值会重复多个地方使用
wxppPropagateMsgTypeS :: WxppPropagateMsg -> Text
wxppPropagateMsgTypeS (WxppPropagateMsgNews {})     = "mpnews"
wxppPropagateMsgTypeS (WxppPropagateMsgText {})     = "text"
wxppPropagateMsgTypeS (WxppPropagateMsgVoice {})    = "voice"
wxppPropagateMsgTypeS (WxppPropagateMsgImage {})    = "image"
wxppPropagateMsgTypeS (WxppPropagateMsgVideo {})    = "video"
wxppPropagateMsgTypeS (WxppPropagateMsgCard {})     = "card"


-- | 为群发而上传图文消息素材
wxppPropagateUploadNews :: (WxppApiMonad env m)
                        => AccessToken
                        -> WxppBriefNews
                            -- ^ XXX: 这里只是用了与永久图文素材相同的数据类型
                            --        真正上传的结果只是一个临时的素材
                        -> m WxppBriefMediaID
wxppPropagateUploadNews (AccessToken { accessTokenData = atk }) news = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/media/uploadnews"
        opts = defaults & param "access_token" .~ [ atk ]

    PUploadNewsResult _mtype media_id _ <-
        liftIO (WS.postWith opts sess url $ toJSON news)
            >>= asWxppWsResponseNormal'
    return media_id


-- | wxppPropagateNewVideoID 内部用
data CreateVideoMediaIDResult = CreateVideoMediaIDResult
                                    Text
                                    WxppPropagateVideoMediaID
                                    UTCTime

instance FromJSON CreateVideoMediaIDResult where
    parseJSON = withObject "CreateVideoMediaIDResult" $ \o -> do
                    CreateVideoMediaIDResult <$> o .: "type"
                        <*> o .: "media_id"
                        <*> (epochIntToUtcTime <$> o .: "created_at")


-- | 文档说：群发视频之前，先上传多媒体文件，得到一个media_id
--           然后还要再调用一个特别接口，得到一个新的 media_id
--           即这里的 WxppPropagateVideoMediaID
wxppPropagateNewVideoID :: (WxppApiMonad env m)
                        => AccessToken
                        -> WxppMediaID
                            -- ^ 通过基础支持中的上传下载多媒体文件得到的id
                        -> Text     -- ^ title
                        -> Text     -- ^ description
                        -> m WxppPropagateVideoMediaID
wxppPropagateNewVideoID (AccessToken { accessTokenData = atk }) media_id title desc = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfFileApiBase url_conf <> "/media/uploadvideo"
        opts = defaults & param "access_token" .~ [ atk ]

    CreateVideoMediaIDResult _typ v_media_id _created_at
        <- liftIO (WS.postWith opts sess url $ object
                    [ "type" .= ("video" :: Text)
                    , "media_id" .= media_id
                    , "title" .= title
                    , "description" .= desc
                    ])
            >>= asWxppWsResponseNormal'
    return v_media_id


newtype PropagateMsgID = PropagateMsgID { unPropagateMsgID :: Word64 }
                    deriving (Show, Eq, Ord)

instance PersistField PropagateMsgID where
    toPersistValue      = toPersistValue . unPropagateMsgID
    fromPersistValue    = fmap PropagateMsgID . fromPersistValue

instance PersistFieldSql PropagateMsgID where
    sqlType _ = SqlInt64

instance ToJSON PropagateMsgID where
    toJSON = toJSON . unPropagateMsgID

instance FromJSON PropagateMsgID where
    parseJSON = fmap PropagateMsgID . parseJSON


-- | 群发接口中的 filter 参数
newtype PropagateFilter = PropagateFilter (Maybe WxppUserGroupID)

instance ToJSON PropagateFilter where
    toJSON (PropagateFilter Nothing)        = object [ "is_to_all" .= True
                                                     , "group_id"  .= ("" :: Text)
                                                     ]
    toJSON (PropagateFilter (Just grp_id))  = object [ "is_to_all" .= False
                                                     , "group_id"  .= show grp_id
                                                     ]

newtype PropagateCallResult = PropagateCallResult PropagateMsgID

instance FromJSON PropagateCallResult where
    parseJSON = withObject "PropagateCallResult" $ \o -> do
                    PropagateCallResult <$> o .: "msg_id"

-- | 群发已准备好的消息
wxppPropagateMsg :: (WxppApiMonad env m)
                 => AccessToken
                 -> Maybe WxppUserGroupID
                 -> WxppPropagateMsg
                 -> m PropagateMsgID
wxppPropagateMsg (AccessToken { accessTokenData = atk }) m_grp_id p_msg = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/sendall"
        opts = defaults & param "access_token" .~ [ atk ]
    let post_v = object $
                    [
                    "msgtype"   .= wxppPropagateMsgTypeS p_msg
                    , "filter"  .= PropagateFilter m_grp_id
                    ] ++
                    [ wxppPropagateMsgTypeS p_msg .= object (wxppPropagateMsgJsonData p_msg) ]

    (_, PropagateCallResult msg_id) <-
        liftIO (WS.postWith opts sess url post_v)
            >>= asWxppWsResponseNormal2'
    return msg_id


-- | 預览要群发的消息
wxppPreviewPropagateMsg :: (WxppApiMonad env m)
                        => AccessToken
                        -> Either WxppOpenID WeixinUserName
                        -> WxppPropagateMsg
                        -> m ()
wxppPreviewPropagateMsg (AccessToken { accessTokenData = atk }) openid_or_name p_msg = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/preview"
        opts = defaults & param "access_token" .~ [ atk ]
        to_spec = case openid_or_name of
                    Left openid -> "touser" .= openid
                    Right name  -> "towxname" .= name

    let post_v = object $ to_spec :
                    [
                    "msgtype" .= wxppPropagateMsgTypeS p_msg
                    , wxppPropagateMsgTypeS p_msg .= object (wxppPropagateMsgJsonData p_msg)
                    ]

    liftIO (WS.postWith opts sess url post_v)
            >>= asWxppWsResponseVoid



-- | 删除已发出的群发消息
wxppDropPropagateMsg :: (WxppApiMonad env m)
                     => AccessToken
                     -> PropagateMsgID
                     -> m ()
wxppDropPropagateMsg (AccessToken { accessTokenData = atk }) msg_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/delete"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "msg_id" .= msg_id ])
            >>= asWxppWsResponseVoid


data PropagateMsgStatus = PropagateMsgStatus Text

instance FromJSON PropagateMsgStatus where
    parseJSON = withObject "PropagateMsgStatus" $ \o ->
                    PropagateMsgStatus <$> o .: "msg_status"


-- | 查询群发消息的发送状态
wxppGetPropagateMsgStatus :: (WxppApiMonad env m)
                          => AccessToken
                          -> PropagateMsgID
                          -> m PropagateMsgStatus
wxppGetPropagateMsgStatus (AccessToken { accessTokenData = atk }) msg_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/get"
        opts = defaults & param "access_token" .~ [ atk ]

    r <- liftIO $ WS.postWith opts sess url $ object [ "msg_id" .= msg_id ]
    -- $logDebugS wxppLogSource $ LT.toStrict $ decodeUtf8 $ r ^. responseBody
    asWxppWsResponseNormal' r
