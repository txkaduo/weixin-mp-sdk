{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Material
    ( module WeiXin.PublicPlatform.Material
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude hiding (catch)
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson
import Control.Monad.Logger
import Data.SafeCopy
-- import Data.Acid                            (query)
import qualified Data.ByteString.Lazy       as LB
import Control.Monad.Catch                  (catch)
-- import Data.Yaml                            (ParseException)
import Data.Conduit
import qualified Data.Conduit.List          as CL
-- import Data.Conduit.Combinators hiding (null)

import WeiXin.PublicPlatform.Types
-- import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils

-- | 查询永久素材中的图文消息中的文章，出现在从服务器返回的数据中
-- 与 WxppDurableArticle 几乎一样，区别只是
-- 多了 url 字段
data WxppDurableArticleS = WxppDurableArticleS {
                                wxppDurableArticleSA        :: WxppDurableArticle
                                , wxppDurableArticleSUrl    :: UrlText
                                }
                            deriving (Eq)

$(deriveSafeCopy 0 'base ''WxppDurableArticleS)

instance FromJSON WxppDurableArticleS where
    parseJSON = withObject "WxppDurableArticleS" $ \o -> do
                    WxppDurableArticleS
                        <$> parseJSON (Object o)
                        <*> (UrlText <$> o .: "url")


instance ToJSON WxppDurableArticleS where
    toJSON x = object $
                ("url" .= unUrlText (wxppDurableArticleSUrl x))
                    : wppDurableArticleToJsonPairs (wxppDurableArticleSA x)

data DurableUploadResult = DurableUploadResult {
                                durableUploadResultMediaID  :: WxppDurableMediaID
                                , durableUploadResultUrl    :: Maybe UrlText
                                    -- ^ 只有图片素材才会返回 url，但这url只能用于腾讯系的应用
                                    -- （不知道是何时更新的功能）
                                }

instance FromJSON DurableUploadResult where
    parseJSON = withObject "DurableUploadResult" $ \obj ->
                    DurableUploadResult
                        <$> obj .: "media_id"
                        <*> (fmap UrlText <$> obj .:? "url")


-- | 上传本地一个媒体文件，成为永久素材
wxppUploadDurableMediaInternal ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> Maybe (Text, Text)
        -- ^ 上传视频素材所需的额外信息：标题，简介
    -> FilePath         -- ^ the file to upload
    -> m DurableUploadResult
wxppUploadDurableMediaInternal atk mtype m_title_intro fp = do
    wxppUploadDurableMediaInternalLBS atk mtype
        m_title_intro
        (partFileSource "media" fp)


-- | 上传本地一个媒体文件，成为永久素材
wxppUploadDurableMediaInternalLBS ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> Maybe (Text, Text)
        -- ^ 上传视频素材所需的额外信息：标题，简介
    -> Part                 -- ^ the @Part@ of uploaded file
    -> m DurableUploadResult
wxppUploadDurableMediaInternalLBS (AccessToken { accessTokenData = atk }) mtype m_title_intro fpart = do
    let url = wxppRemoteApiBaseUrl <> "/material/add_material"
        opts = defaults & param "access_token" .~ [ atk ]

    (liftIO $ postWith opts url $
                [ fpart & partName .~ "media"
                , partText "type" (wxppMediaTypeString mtype)
                ]
                ++ (case m_title_intro of
                        Nothing             -> []
                        Just (title, intro) ->
                            [ partText "title" title
                            , partText "introduction" intro
                            ]
                )
            )
            >>= asWxppWsResponseNormal'


-- | 上传本地视频成为永久素材
wxppUploadDurableMediaVideo ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Text     -- ^ 标题
    -> Text     -- ^ 简介
    -> FilePath
    -> m DurableUploadResult
wxppUploadDurableMediaVideo atk title intro fp = do
    wxppUploadDurableMediaInternal atk WxppMediaTypeVideo (Just (title, intro)) fp


wxppUploadDurableMediaVideoLBS ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> Text     -- ^ 标题
    -> Text     -- ^ 简介
    -> FilePath -- ^ a dummy file name
    -> LB.ByteString
    -> m DurableUploadResult
wxppUploadDurableMediaVideoLBS atk title intro fp lbs = do
    wxppUploadDurableMediaInternalLBS atk WxppMediaTypeVideo (Just (title, intro))
        (partLBS "media" lbs & partFileName .~ Just fp)


-- | 上传本地非视频媒体成为永久素材
wxppUploadDurableMediaNonVideo ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> FilePath
    -> m DurableUploadResult
wxppUploadDurableMediaNonVideo atk mtype fp = do
    case mtype of
        WxppMediaTypeVideo ->
            throwM $ userError
                "wxppUploadDurableMediaNonVideo cannot be used to upload video."
        _   -> return ()
    wxppUploadDurableMediaInternal atk mtype Nothing fp


-- | 上传本地非视频媒体成为永久素材
wxppUploadDurableMediaNonVideoLBS ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> FilePath
    -> LB.ByteString
    -> m DurableUploadResult
wxppUploadDurableMediaNonVideoLBS atk mtype fp lbs = do
    case mtype of
        WxppMediaTypeVideo ->
            throwM $ userError
                "wxppUploadDurableMediaNonVideo cannot be used to upload video."
        _   -> return ()
    wxppUploadDurableMediaInternalLBS atk mtype Nothing
        (partLBS "media" lbs & partFileName .~ Just fp)


-- | 上传一个图文素材成为永久素材
wxppUploadDurableNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppDurableNews
    -> m DurableUploadResult
wxppUploadDurableNews (AccessToken { accessTokenData = atk }) news = do
    let url = wxppRemoteApiBaseUrl <> "/material/add_news"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ encode $ toJSON news)
            >>= asWxppWsResponseNormal'


-- | 查询永久素材的结果内容完全只能通过尝试的方式决定
-- 混合了多种情况
data WxppGetDurableResult =    WxppGetDurableNews [WxppDurableArticleS]
                            |   WxppGetDurableVideo Text Text UrlText
                                -- ^ title description download_url
                            |   WxppGetDurableRaw ByteString LB.ByteString

instance FromJSON WxppGetDurableResult where
    -- 这个函数不用处理 WxppGetDurableRaw，因为这种情况并非用 JSON 表示
    parseJSON = withObject "WxppGetDurableResult" $ \obj -> do
                    let parse_as_news = WxppGetDurableNews <$> obj .: "news_item"
                        parse_as_video = WxppGetDurableVideo
                                            <$> obj .: "title"
                                            <*> obj .: "description"
                                            <*> (UrlText <$> obj .: "down_url")
                    parse_as_news <|> parse_as_video


-- | 获取永久素材
wxppGetDurableMedia ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> WxppDurableMediaID
    -> m WxppGetDurableResult
wxppGetDurableMedia (AccessToken { accessTokenData = atk }) (WxppDurableMediaID mid) = do
    let url = wxppRemoteApiBaseUrl <> "/material/get_material"
        opts = defaults & param "access_token" .~ [ atk ]
    r <- liftIO $ postWith opts url $ object [ "media_id" .= mid ]
    asWxppWsResponseNormal' r
        `catch`
        (\(_ :: JSONError) -> do
            return $ WxppGetDurableRaw
                        (r ^. responseHeader "Content-Type")
                        (r ^. responseBody)
            )


-- | 永久素材统计数
data WxppDurableCount = WxppDurableCount {
                            wxppDurableCountVoice      :: Int
                            , wxppDurableCountVideo    :: Int
                            , wxppDurableCountImage    :: Int
                            , wxppDurableCountNews     :: Int
                        }

instance FromJSON WxppDurableCount where
    parseJSON = withObject "WxppDurableCount" $ \obj -> do
                    WxppDurableCount
                        <$> ( obj .: "voice_count" )
                        <*> ( obj .: "video_count" )
                        <*> ( obj .: "image_count" )
                        <*> ( obj .: "news_count" )

instance ToJSON WxppDurableCount where
    toJSON x = object
                [ "voice_count" .= wxppDurableCountVoice x
                , "video_count" .= wxppDurableCountVideo x
                , "image_count" .= wxppDurableCountImage x
                , "news_count"  .= wxppDurableCountNews x
                ]

wxppCountDurableMedia ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> m WxppDurableCount
wxppCountDurableMedia (AccessToken { accessTokenData = atk }) = do
    let url = wxppRemoteApiBaseUrl <> "/material/get_materialcount"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ getWith opts url)
            >>= asWxppWsResponseNormal'


-- | 批量取永久素材接口，无论是图文还是一般的多媒体
-- 返回报文都是这样子的结构
data WxppBatchGetDurableResult a = WxppBatchGetDurableResult {
                                        wxppBatchGetDurableResultTotal     :: Int
                                        , wxppBatchGetDurableResultCount   :: Int
                                        , wxppBatchGetDurableResultItems   :: [a]
                                    }

instance FromJSON a => FromJSON (WxppBatchGetDurableResult a) where
    parseJSON = withObject "WxppBatchGetDurableResult" $ \obj -> do
                    WxppBatchGetDurableResult
                        <$> ( obj .: "total_count" )
                        <*> ( obj .: "item_count" )
                        <*> ( obj .: "item" )

-- | 批量取多媒体类型的永久素材，返回报文中的项目
data WxppBatchGetDurableMediaItem = WxppBatchGetDurableMediaItem {
                                        wxppBatchGetDurableMediaItemID     :: WxppDurableMediaID
                                        , wxppBatchGetDurableMediaItemName :: Text
                                        , wxppBatchGetDurableMediaItemUrl  :: Maybe UrlText
                                        , wxppBatchGetDurableMediaItemTime :: UTCTime
                                    }

$(deriveSafeCopy 0 'base ''WxppBatchGetDurableMediaItem)

instance FromJSON WxppBatchGetDurableMediaItem where
    parseJSON = withObject "WxppBatchGetDurableMediaItem" $ \obj -> do
                    WxppBatchGetDurableMediaItem
                        <$> ( obj .: "media_id" )
                        <*> ( obj .: "name")
                        <*> (fmap UrlText <$> obj .:? "url")
                        <*> ( epochIntToUtcTime <$> obj .: "update_time")

instance ToJSON WxppBatchGetDurableMediaItem where
    toJSON x = object
                    [ "media_id" .= wxppBatchGetDurableMediaItemID x
                    , "name" .= wxppBatchGetDurableMediaItemName x
                    , "update_time" .= wxppBatchGetDurableMediaItemTime x
                    ]

-- | 调用批量取多媒体类型的永久素材的接口
wxppBatchGetDurableMedia ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> WxppMediaType
    -> Int      -- ^ limit
    -> Int      -- ^ offset
    -> m (WxppBatchGetDurableResult WxppBatchGetDurableMediaItem)
wxppBatchGetDurableMedia (AccessToken { accessTokenData = atk }) mtype limit' offset' = do
    let url = wxppRemoteApiBaseUrl <> "/material/batchget_material"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object $
                [ "type"    .= (wxppMediaTypeString mtype :: Text)
                , "count"   .= show limit
                , "offset"  .= show offset
                ])
            >>= asWxppWsResponseNormal'
    where
        limit = max 1 $ min 20 $ limit'
        offset = max 0 $ offset'


-- | 批量取图文类型的永久素材，返回报文中的项目
data WxppBatchGetDurableNewsItem = WxppBatchGetDurableNewsItem {
                                        wxppBatchGetDurableNewsItemID          :: WxppDurableMediaID
                                        , wxppBatchGetDurableNewsItemContent   :: [WxppDurableArticleS]
                                        , wxppBatchGetDurableNewsItemTime      :: UTCTime
                                    }
                                    deriving (Eq)

$(deriveSafeCopy 0 'base ''WxppBatchGetDurableNewsItem)

instance FromJSON WxppBatchGetDurableNewsItem where
    parseJSON = withObject "WxppBatchGetDurableNewsItem" $ \obj -> do
                    WxppBatchGetDurableNewsItem
                        <$> ( obj .: "media_id" )
                        <*> ( obj .: "content" >>= (\o -> o .: "news_item"))
                        <*> ( epochIntToUtcTime <$> obj .: "update_time")

instance ToJSON WxppBatchGetDurableNewsItem where
    toJSON x = object
                    [ "media_id" .= wxppBatchGetDurableNewsItemID x
                    , "content" .= object [ "news_item" .= wxppBatchGetDurableNewsItemContent x ]
                    , "update_time" .= utcTimeToEpochInt (wxppBatchGetDurableNewsItemTime x)
                    ]

-- | 调用批量取图文消息类型的永久素材的接口
wxppBatchGetDurableNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> Int      -- ^ limit
    -> Int      -- ^ offset
    -> m (WxppBatchGetDurableResult WxppBatchGetDurableNewsItem)
wxppBatchGetDurableNews (AccessToken { accessTokenData = atk }) limit' offset' = do
    let url = wxppRemoteApiBaseUrl <> "/material/batchget_material"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object $
                [ "type"    .= ("news" :: Text)
                , "count"   .= limit
                , "offset"  .= offset
                ])
            >>= asWxppWsResponseNormal'
    where
        limit = max 1 $ min 20 $ limit'
        offset = max 0 $ offset'

-- | 把 limit/offset 风格的接口变成 conduit 风格：取全部数据
wxppBatchGetDurableToSrc' ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    (Int
        -> m (WxppBatchGetDurableResult a)
    )   -- ^ 这个函数可以从前面的函数应用上部分参数后得到, Int 参数是 offset
    -> Source m (WxppBatchGetDurableResult a)
wxppBatchGetDurableToSrc' get_by_offset = go 0
    where
        go offset = do
            result <- lift $ get_by_offset offset
            let items = wxppBatchGetDurableResultItems result
            if null items
                then return ()
                else do
                    yield result
                    let batch_count = wxppBatchGetDurableResultCount result
                    go $ offset + batch_count

wxppBatchGetDurableToSrc ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    (Int
        -> m (WxppBatchGetDurableResult a)
    )   -- ^ 这个函数可以从前面的函数应用上部分参数后得到, Int 参数是 offset
    -> Source m a
wxppBatchGetDurableToSrc get_by_offset =
    wxppBatchGetDurableToSrc' get_by_offset =$ CL.concatMap wxppBatchGetDurableResultItems

-- | 修改一个图文类型的永久素材: 只能修改其中一个文章
wxppReplaceArticleOfDurableNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m ) =>
    AccessToken
    -> WxppDurableMediaID
    -> Int      -- ^ index
    -> WxppDurableArticle
    -> m ()
wxppReplaceArticleOfDurableNews (AccessToken { accessTokenData = atk }) material_id idx article = do
    let url = wxppRemoteApiBaseUrl <> "/material/update_news"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object $
                [ "media_id"    .= unWxppDurableMediaID material_id
                , "index"       .= idx
                , "articles"    .= article
                ])
    >>= asWxppWsResponseVoid
