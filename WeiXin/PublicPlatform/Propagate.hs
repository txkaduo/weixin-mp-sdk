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
    , wxppPropagateMsgOpenIDMaxNum, wxppPropagateMsgToOpenIDs
    , wxppPreviewPropagateMsg
    , wxppDropPropagatedMsg
    , PropagateFilter(..)
    , wxppGetPropagateMsgStatus
    , PropagateMsgStatus(..)
    ) where

-- {{{1 imports
import ClassyPrelude
-- import qualified Data.Text.Lazy             as LT
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Control.Monad.Reader                 (asks)
import Data.Aeson
import Data.Aeson.Types                     (Pair)
import Data.List.NonEmpty                   (NonEmpty)
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..)
                                            , SqlType(..))

import Yesod.Helpers.Utils                  (nullToNothing)
import Yesod.Helpers.Aeson                  (parseIntWithTextparsec)
import Yesod.Helpers.Parsec                 (natural)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils
-- }}}1


-- | 准备群发的图文素材结构中的一个文章
-- 从接口的URL及网上文章的讨论看，所谓群发上传图文消息接口估计
-- 本来是“上传临时图文素材”（注意url的media单词）
-- 所上传的每个article内容与 WxppDurableArticle 实质上是一样的
-- 只有一个区别：封面图片必须是临时素材media_id
-- （有传闻说永久素材与临时素材id不能混用）
data WxppBriefArticle =
       WxppBriefArticle
         { wxppBriefArticleTitle              :: Text
         , wxppBriefArticleThumb              :: WxppBriefMediaID
         , wxppBriefArticleAuthor             :: Maybe Text
         , wxppBriefArticleDigest             :: Maybe Text
         , wxppBriefArticleContent            :: Text
         , wxppBriefArticleContentSrcUrl      :: Maybe UrlText
         , wxppBriefArticleShowCoverPic       :: Maybe Bool
         , wxppBriefArticleNeedOpenComment    :: Maybe Bool
         , wxppBriefArticleOnlyFansCanComment :: Maybe Bool
         }
  deriving (Eq, Show)


-- {{{1 instances
instance FromJSON WxppBriefArticle where
    parseJSON = withObject "WxppBriefArticle" $ \ obj -> do
                    WxppBriefArticle
                        <$> ( obj .: "title" )
                        <*> ( obj .: "thumb_media_id" )
                        <*> ( join . fmap nullToNothing <$> obj .:? "author" )
                        <*> ( join . fmap nullToNothing <$> obj .:? "digest" )
                        <*> ( obj .: "content" )
                        <*> ( fmap UrlText . join . fmap nullToNothing <$>
                                obj .:? "content_source_url" )
                        <*> optional_int_bool obj "show_cover_pic"
                        <*> optional_int_bool obj "need_open_comment"
                        <*> optional_int_bool obj "only_fans_can_comment"
            where
                int_to_bool x = x /= 0

                optional_int_bool obj field =
                  obj .:? field
                    >>= mapM (parseIntWithTextparsec natural)
                    >>= return . fmap int_to_bool


instance ToJSON WxppBriefArticle where
    toJSON x = object $ catMaybes
              [ Just $ "title"           .= wxppBriefArticleTitle x
              , Just $ "thumb_media_id"  .= wxppBriefArticleThumb x
              , Just $ "author"          .= (fromMaybe "" $ wxppBriefArticleAuthor x)
              , Just $ "digest"          .= (fromMaybe "" $ wxppBriefArticleDigest x)
              , Just $ "content"         .= wxppBriefArticleContent x
              , Just $ "content_source_url" .= (fromMaybe "" $ unUrlText <$> wxppBriefArticleContentSrcUrl x)
              , optional_bool_kv "show_cover_pic" wxppBriefArticleShowCoverPic
              , optional_bool_kv "need_open_comment" wxppBriefArticleNeedOpenComment
              , optional_bool_kv "only_fans_can_comment" wxppBriefArticleOnlyFansCanComment
              ]
            where
                optional_bool_kv field getf =
                  (field .=) . show . boolToInt <$> getf x
-- }}}1


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
data WxppPropagateMsg = WxppPropagateMsgNews WxppMediaID Bool
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
                        "mpnews" -> WxppPropagateMsgNews <$> o .: "media_id" <*> o .:? "send_ignore_reprint" .!= False
                        "text"  -> WxppPropagateMsgText <$> o .: "content"
                        "voice" -> WxppPropagateMsgVoice <$> o .: "media_id"
                        "image" -> WxppPropagateMsgImage <$> o .: "media_id"
                        "mpvideo" -> WxppPropagateMsgVideo <$> o .: "media_id"
                        "wxcard" -> WxppPropagateMsgCard <$> o .: "card_id"
                        _ -> fail $ "unknown type in WxppPropagateMsg: " <> typ


instance ToJSON WxppPropagateMsg where
    toJSON x = object $ ("type" .= wxppPropagateMsgTypeS x) : wxppPropagateMsgJsonData x


-- | 这是群发时提交数据的一部分
wxppPropagateMsgJsonData :: WxppPropagateMsg -> [ Pair ]
wxppPropagateMsgJsonData x =
    case x of
        WxppPropagateMsgNews media_id _ -> [ "media_id" .= media_id ]
        WxppPropagateMsgText t          -> [ "content"  .= t ]
        WxppPropagateMsgVoice media_id  -> [ "media_id" .= media_id ]
        WxppPropagateMsgImage media_id  -> [ "media_id" .= media_id ]
        WxppPropagateMsgVideo media_id  -> [ "media_id" .= media_id ]
        WxppPropagateMsgCard card_id    -> [  "card_id" .= card_id ]



-- | 这是群发时提交数据的一部分，在json的顶层部分，目前只有图文消息有此需求
wxppPropagateMsgJsonDataTop :: WxppPropagateMsg -> [ Pair ]
wxppPropagateMsgJsonDataTop (WxppPropagateMsgNews _ send_ignore_reprint) = [ "send_ignore_reprint" .= send_ignore_reprint ]
wxppPropagateMsgJsonDataTop _ = []


-- | 这个字段值会重复多个地方使用
wxppPropagateMsgTypeS :: WxppPropagateMsg -> Text
wxppPropagateMsgTypeS (WxppPropagateMsgNews {})     = "mpnews"
wxppPropagateMsgTypeS (WxppPropagateMsgText {})     = "text"
wxppPropagateMsgTypeS (WxppPropagateMsgVoice {})    = "voice"
wxppPropagateMsgTypeS (WxppPropagateMsgImage {})    = "image"
wxppPropagateMsgTypeS (WxppPropagateMsgVideo {})    = "mpvideo"
wxppPropagateMsgTypeS (WxppPropagateMsgCard {})     = "wxcard"


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
                    [ "type" .= ("video" :: Text) -- ^ FIXME: 这个字段文档不见，不知道是否多余
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
newtype PropagateFilter = PropagateFilter (Maybe WxppUserTagID)

instance ToJSON PropagateFilter where
    toJSON (PropagateFilter Nothing)        = object [ "is_to_all" .= True
                                                     , "tag_id"  .= ("" :: Text)
                                                     ]
    toJSON (PropagateFilter (Just tag_id))  = object [ "is_to_all" .= False
                                                     , "tag_id"  .= tshow (unWxppUserTagID tag_id)
                                                     ]

newtype PropagateCallResult = PropagateCallResult PropagateMsgID
-- TODO: 文档显示，现在还增加返回多一个字段 msg_data_id

instance FromJSON PropagateCallResult where
    parseJSON = withObject "PropagateCallResult" $ \o -> do
                    PropagateCallResult <$> o .: "msg_id"

-- | 群发已准备好的消息: 全体或指定标签
wxppPropagateMsg :: (WxppApiMonad env m)
                 => AccessToken
                 -> Maybe WxppUserTagID
                 -> WxppPropagateMsg
                 -> m PropagateMsgID
-- {{{1
wxppPropagateMsg (AccessToken { accessTokenData = atk }) m_tag_id p_msg = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/sendall"
        opts = defaults & param "access_token" .~ [ atk ]
    let post_v = object $
                    [
                    "msgtype"   .= wxppPropagateMsgTypeS p_msg
                    , "filter"  .= PropagateFilter m_tag_id
                    ]
                      <> [ wxppPropagateMsgTypeS p_msg .= object (wxppPropagateMsgJsonData p_msg) ]
                      <> wxppPropagateMsgJsonDataTop p_msg

    (_, PropagateCallResult msg_id) <-
        liftIO (WS.postWith opts sess url post_v)
            >>= asWxppWsResponseNormal2'
    return msg_id
-- }}}1


wxppPropagateMsgOpenIDMaxNum :: Int
wxppPropagateMsgOpenIDMaxNum = 10000


-- | 群发已准备好的消息: 全体或指定标签
wxppPropagateMsgToOpenIDs :: (WxppApiMonad env m)
                          => AccessToken
                          -> NonEmpty WxppOpenID  -- ^ 最少 2 个，最多 100000 个
                          -> WxppPropagateMsg
                          -> m PropagateMsgID
-- {{{1
wxppPropagateMsgToOpenIDs (AccessToken { accessTokenData = atk }) open_ids p_msg = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/send"
        opts = defaults & param "access_token" .~ [ atk ]
    let post_v = object $
                    [
                    "msgtype"   .= wxppPropagateMsgTypeS p_msg
                    , "touser"  .= toList open_ids
                    ]
                      <> [ wxppPropagateMsgTypeS p_msg .= object (wxppPropagateMsgJsonData p_msg) ]
                      <> wxppPropagateMsgJsonDataTop p_msg

    (_, PropagateCallResult msg_id) <-
        liftIO (WS.postWith opts sess url post_v)
            >>= asWxppWsResponseNormal2'
    return msg_id
-- }}}1


-- | 预览要群发的消息
wxppPreviewPropagateMsg :: (WxppApiMonad env m)
                        => AccessToken
                        -> Either WxppOpenID WeixinUserName
                        -> WxppPropagateMsg
                        -> m ()
-- {{{1
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
                      <> wxppPropagateMsgJsonDataTop p_msg

    liftIO (WS.postWith opts sess url post_v)
            >>= asWxppWsResponseVoid
-- }}}1


-- | 删除已发出的群发消息
wxppDropPropagatedMsg :: (WxppApiMonad env m)
                      => AccessToken
                      -> PropagateMsgID
                      -> Maybe Int
                      -> m ()
-- {{{1
wxppDropPropagatedMsg (AccessToken { accessTokenData = atk }) msg_id m_article_idx = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/delete"
        opts = defaults & param "access_token" .~ [ atk ]
        post_data = object $ catMaybes $
                      [ Just $ "msg_id" .= msg_id
                      , ("article_idx" .=) <$> m_article_idx
                      ]

    liftIO (WS.postWith opts sess url post_data)
            >>= asWxppWsResponseVoid
-- }}}1


data PropagateMsgStatus = PropagateMsgStatus Text

instance FromJSON PropagateMsgStatus where
    parseJSON = withObject "PropagateMsgStatus" $ \o ->
                    PropagateMsgStatus <$> o .: "msg_status"


-- | 查询群发消息的发送状态
wxppGetPropagateMsgStatus :: (WxppApiMonad env m)
                          => AccessToken
                          -> PropagateMsgID
                          -> m PropagateMsgStatus
-- {{{1
wxppGetPropagateMsgStatus (AccessToken { accessTokenData = atk }) msg_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/message/mass/get"
        opts = defaults & param "access_token" .~ [ atk ]

    r <- liftIO $ WS.postWith opts sess url $ object [ "msg_id" .= msg_id ]
    --- $logDebugS wxppLogSource $ LT.toStrict $ decodeUtf8 $ r ^. responseBody
    asWxppWsResponseNormal' r
-- }}}1


-- vim: set foldmethod=marker:
