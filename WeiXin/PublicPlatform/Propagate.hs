module WeiXin.PublicPlatform.Propagate
    ( wxppPropagateUploadNews
    , PropagateMsgID(..)
    , wxppPropagateNews
    , wxpPreviewPropagateNews
    , wxppPropagateText
    , wxpPreviewPropagateText
    , wxppDropPropagateMsg
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Data.Aeson
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..)
                                            , SqlType(..))

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils


data PUploadNewsResult = PUploadNewsResult
                            Text
                            WxppBriefMediaID
                            UTCTime

instance FromJSON PUploadNewsResult where
    parseJSON = withObject "PUploadNewsResult" $ \o -> do
                    PUploadNewsResult   <$> o .: "type"
                                        <*> o .: "media_id"
                                        <*> (fmap epochIntToUtcTime $ o .: "created_at")

-- | 为群发而上传图文消息素材
wxppPropagateUploadNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppDurableNews
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

-- | 群发已上传的图文消息
wxppPropagateNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Maybe WxppUserGroupID
    -> WxppMediaID
        -- ^ 按文档，这个 media_id 须使用 wxppPropagateUploadNews 取得
        -- 或使用永久素材的 media_id
    -> m PropagateMsgID
wxppPropagateNews (AccessToken { accessTokenData = atk }) m_grp_id media_id = do
    let url = wxppRemoteApiBaseUrl <> "/message/mass/sendall"
        opts = defaults & param "access_token" .~ [ atk ]
    let post_v = object [
                    "msgtype"   .= ("mpnews" :: Text)
                    , "mpnews"  .= object [ "media_id" .= media_id ]
                    , "filter"  .= PropagateFilter m_grp_id
                    ]
    (_, PropagateCallResult msg_id) <-
        (liftIO $ postWith opts url post_v)
            >>= asWxppWsResponseNormal2'
    return msg_id


-- | 預览要群发的图文消息
wxpPreviewPropagateNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Either WxppOpenID WeixinUserName
    -> WxppMediaID
    -> m ()
wxpPreviewPropagateNews (AccessToken { accessTokenData = atk }) openid_or_name media_id = do
    let url = wxppRemoteApiBaseUrl <> "/message/mass/preview"
        opts = defaults & param "access_token" .~ [ atk ]
        to_spec = case openid_or_name of
                    Left openid -> "touser" .= openid
                    Right name  -> "towxname" .= name

    let post_v = object $ to_spec :
                    [
                    "msgtype"   .= ("mpnews" :: Text)
                    , "mpnews"  .= object [ "media_id" .= media_id ]
                    ]

    (liftIO $ postWith opts url post_v)
            >>= asWxppWsResponseVoid


-- | 群发文本消息
wxppPropagateText ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Maybe WxppUserGroupID
    -> Text
    -> m PropagateMsgID
wxppPropagateText (AccessToken { accessTokenData = atk }) m_grp_id txt = do
    let url = wxppRemoteApiBaseUrl <> "/message/mass/sendall"
        opts = defaults & param "access_token" .~ [ atk ]
    let post_v = object [
                    "msgtype"   .= ("text" :: Text)
                    , "text"    .= object [ "content" .= txt ]
                    , "filter"  .= PropagateFilter m_grp_id
                    ]
    (_, PropagateCallResult msg_id) <-
        (liftIO $ postWith opts url post_v)
            >>= asWxppWsResponseNormal2'
    return msg_id


-- | 預览要群发的文本消息
wxpPreviewPropagateText ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Either WxppOpenID WeixinUserName
    -> Text
    -> m ()
wxpPreviewPropagateText (AccessToken { accessTokenData = atk }) openid_or_name txt = do
    let url = wxppRemoteApiBaseUrl <> "/message/mass/preview"
        opts = defaults & param "access_token" .~ [ atk ]
        to_spec = case openid_or_name of
                    Left openid -> "touser" .= openid
                    Right name  -> "towxname" .= name

    let post_v = object $ to_spec :
                    [
                    "msgtype"   .= ("text" :: Text)
                    , "text"    .= object [ "content" .= txt ]
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
