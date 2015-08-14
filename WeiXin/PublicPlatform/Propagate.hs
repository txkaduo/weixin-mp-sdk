{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Propagate
    ( wxppPropagateUploadNews
    , WxppPropagateArticle(..)
    , wxppDurableToPropagateArticle
    , WxppPropagateNews(..)
    , wxppDurableToPropagateNews
    , PropagateMsgID(..)
    , WxppPropagateMsg(..)
    , WxppPropagateVideoMediaID(..)
    , wxppPropagateNewVideoID
    , wxppPropagateMsg
    , wxppPreviewPropagateMsg
    , wxppDropPropagateMsg
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types                     (Pair)
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..)
                                            , SqlType(..))

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils


-- | 准备群发的图文素材结构中的一个文章
-- 这个结构基本上与 WxppDurableArticle 一样，区别是
-- - media id 换成了 WxppMediaID
data WxppPropagateArticle = WxppPropagateArticle {
                                wxppPropagateArticleTitle            :: Text
                                , wxppPropagateArticleThumb          :: WxppMediaID
                                , wxppPropagateArticleAuthor         :: Maybe Text
                                , wxppPropagateArticleDigest         :: Maybe Text
                                , wxppPropagateArticleShowCoverPic   :: Bool
                                , wxppPropagateArticleContent        :: Text
                                , wxppPropagateArticleContentSrcUrl  :: Maybe UrlText
                            }
                            deriving (Eq)

wxppDurableToPropagateArticle :: WxppDurableArticle -> WxppPropagateArticle
wxppDurableToPropagateArticle x = WxppPropagateArticle
                                    (wxppDurableArticleTitle x)
                                    (fromWxppDurableMediaID $ wxppDurableArticleThumb x)
                                    (wxppDurableArticleAuthor x)
                                    (wxppDurableArticleDigest x)
                                    (wxppDurableArticleShowCoverPic x)
                                    (wxppDurableArticleContent x)
                                    (wxppDurableArticleContentSrcUrl x)

instance FromJSON WxppPropagateArticle where
    parseJSON = withObject "WxppPropagateArticle" $ \obj -> do
                    WxppPropagateArticle
                        <$> ( obj .: "title" )
                        <*> ( obj .: "thumb_media_id" )
                        <*> ( obj .:? "author" )
                        <*> ( obj .:? "digest" )
                        <*> ( int_to_bool <$> obj .: "show_cover_pic" )
                        <*> ( obj .: "content" )
                        <*> ( fmap UrlText <$> obj .:? "content_source_url" )
            where
                int_to_bool x = (x :: Int) /= 0


instance ToJSON WxppPropagateArticle where
    toJSON x = object   [ "title"           .= wxppPropagateArticleTitle x
                        , "thumb_media_id"  .= wxppPropagateArticleThumb x
                        , "author"          .= (fromMaybe "" $ wxppPropagateArticleAuthor x)
                        , "digest"          .= (fromMaybe "" $ wxppPropagateArticleDigest x)
                        , "show_cover_pic"  .= bool_to_int (wxppPropagateArticleShowCoverPic x)
                        , "content"         .= wxppPropagateArticleContent x
                        , "content_source_url" .= (unUrlText <$> wxppPropagateArticleContentSrcUrl x)
                        ]
            where
                bool_to_int b = if b then 1 :: Int else 0


-- | 可以群发的图文消息
newtype WxppPropagateNews = WxppPropagateNews [WxppPropagateArticle]

wxppDurableToPropagateNews :: WxppDurableNews -> WxppPropagateNews
wxppDurableToPropagateNews (WxppDurableNews articles) =
                        WxppPropagateNews $ map wxppDurableToPropagateArticle articles

instance FromJSON WxppPropagateNews where
    parseJSON = withObject "WxppPropagateNews" $ \obj -> do
                    WxppPropagateNews <$> obj .: "articles"

instance ToJSON WxppPropagateNews where
    toJSON (WxppPropagateNews articles) = object [ "articles" .= articles ]


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
wxppPropagateMsgTypeS (WxppPropagateMsgNews {})     = "mp_news"
wxppPropagateMsgTypeS (WxppPropagateMsgText {})     = "text"
wxppPropagateMsgTypeS (WxppPropagateMsgVoice {})    = "voice"
wxppPropagateMsgTypeS (WxppPropagateMsgImage {})    = "image"
wxppPropagateMsgTypeS (WxppPropagateMsgVideo {})    = "video"
wxppPropagateMsgTypeS (WxppPropagateMsgCard {})     = "card"


-- | 为群发而上传图文消息素材
wxppPropagateUploadNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppPropagateNews
        -- ^ XXX: 这里只是用了与永久图文素材相同的数据类型
        --        真正上传的结果只是一个临时的素材
    -> m WxppBriefMediaID
wxppPropagateUploadNews (AccessToken { accessTokenData = atk }) news = do
    let url = wxppRemoteApiBaseUrl <> "/media/uploadnews"
        opts = defaults & param "access_token" .~ [ atk ]
    PUploadNewsResult _mtype media_id _ <-
        (liftIO $ postWith opts url $ toJSON news)
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
wxppPropagateNewVideoID ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaID
        -- ^ 通过基础支持中的上传下载多媒体文件得到的id
    -> Text     -- ^ title
    -> Text     -- ^ description
    -> m WxppPropagateVideoMediaID
wxppPropagateNewVideoID (AccessToken { accessTokenData = atk }) media_id title desc = do
    let url = "https://file.api.weixin.qq.com/cgi-bin/media/uploadvideo"
        opts = defaults & param "access_token" .~ [ atk ]
    CreateVideoMediaIDResult _typ v_media_id _created_at
        <- (liftIO $ postWith opts url $ object
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
    toJSON (PropagateFilter Nothing)        = object [ "is_to_all" .= True ]
    toJSON (PropagateFilter (Just grp_id))  = object [ "is_to_all" .= False
                                                     , "group_id"  .= grp_id
                                                     ]

newtype PropagateCallResult = PropagateCallResult PropagateMsgID

instance FromJSON PropagateCallResult where
    parseJSON = withObject "PropagateCallResult" $ \o -> do
                    PropagateCallResult <$> o .: "msg_id"

-- | 群发已准备好的消息
wxppPropagateMsg ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Maybe WxppUserGroupID
    -> WxppPropagateMsg
    -> m PropagateMsgID
wxppPropagateMsg (AccessToken { accessTokenData = atk }) m_grp_id p_msg = do
    let url = wxppRemoteApiBaseUrl <> "/message/mass/sendall"
        opts = defaults & param "access_token" .~ [ atk ]
    let post_v = object $
                    [
                    "msgtype"   .= wxppPropagateMsgTypeS p_msg
                    , "filter"  .= PropagateFilter m_grp_id
                    ] ++
                    [ wxppPropagateMsgTypeS p_msg .= object (wxppPropagateMsgJsonData p_msg) ]

    (_, PropagateCallResult msg_id) <-
        (liftIO $ postWith opts url post_v)
            >>= asWxppWsResponseNormal2'
    return msg_id


-- | 預览要群发的消息
wxppPreviewPropagateMsg ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Either WxppOpenID WeixinUserName
    -> WxppPropagateMsg
    -> m ()
wxppPreviewPropagateMsg (AccessToken { accessTokenData = atk }) openid_or_name p_msg = do
    let url = wxppRemoteApiBaseUrl <> "/message/mass/preview"
        opts = defaults & param "access_token" .~ [ atk ]
        to_spec = case openid_or_name of
                    Left openid -> "touser" .= openid
                    Right name  -> "towxname" .= name

    let post_v = object $ to_spec :
                    [
                    "msgtype" .= wxppPropagateMsgTypeS p_msg
                    , wxppPropagateMsgTypeS p_msg .= object (wxppPropagateMsgJsonData p_msg)
                    ]

    (liftIO $ postWith opts url post_v)
            >>= asWxppWsResponseVoid



-- | 删除已发出的群发消息
wxppDropPropagateMsg ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> PropagateMsgID
    -> m ()
wxppDropPropagateMsg (AccessToken { accessTokenData = atk }) msg_id = do
    let url = wxppRemoteApiBaseUrl <> "/message/mass/delete"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object [ "msg_id" .= msg_id ])
            >>= asWxppWsResponseVoid
