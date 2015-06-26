{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Material
    ( module WeiXin.PublicPlatform.Material
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude hiding (catch, FilePath, (<.>), (</>))
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson
import Control.Monad.Logger
import Filesystem.Path.CurrentOS            (encodeString, FilePath)
-- import Data.Acid                            (query)
import qualified Data.ByteString.Lazy       as LB
import Control.Monad.Catch                  (catch)
-- import Data.Yaml                            (ParseException)
import Data.Conduit
import Data.Conduit.Combinators hiding (null)

import WeiXin.PublicPlatform.Types
-- import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils


newtype MaterialUploadResult = MaterialUploadResult WxppMaterialID

instance FromJSON MaterialUploadResult where
    parseJSON = withObject "MaterialUploadResult" $ \obj ->
                    MaterialUploadResult <$> obj .: "media_id"


-- | 上传本地一个媒体文件，成为永久素材
wxppUploadMaterialMediaInternal ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> FilePath
    -> Maybe (Text, Text)
        -- ^ 上传视频素材所需的额外信息：标题，简介
    -> m MaterialUploadResult
wxppUploadMaterialMediaInternal (AccessToken { accessTokenData = atk }) mtype fp m_title_intro = do
    let url = wxppRemoteApiBaseUrl <> "/material/add_material"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $
                [ partFileSource "media" $ encodeString fp
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
wxppUploadMaterialMediaVideo ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> FilePath
    -> Text     -- ^ 标题
    -> Text     -- ^ 简介
    -> m MaterialUploadResult
wxppUploadMaterialMediaVideo atk fp title intro = do
    wxppUploadMaterialMediaInternal atk WxppMediaTypeVideo fp (Just (title, intro))


-- | 上传本地非视频媒体成为永久素材
wxppUploadMaterialMediaNonVideo ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> FilePath
    -> m MaterialUploadResult
wxppUploadMaterialMediaNonVideo atk mtype fp = do
    case mtype of
        WxppMediaTypeVideo ->
            throwM $ userError
                "wxppUploadMaterialMediaNonVideo cannot be used to upload video."
        _   -> return ()
    wxppUploadMaterialMediaInternal atk mtype fp Nothing


-- | 上传一个图文素材成为永久素材
wxppUploadMaterialNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMaterialNews
    -> m MaterialUploadResult
wxppUploadMaterialNews (AccessToken { accessTokenData = atk }) news = do
    let url = wxppRemoteApiBaseUrl <> "/material/add_news"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ encode $ toJSON news)
            >>= asWxppWsResponseNormal'


-- | 查询永久素材的结果内容完全只能通过尝试的方式决定
-- 混合了多种情况
data WxppGetMaterialResult =    WxppGetMaterialNews [WxppMaterialArticle]
                            |   WxppGetMaterialVideo Text Text UrlText
                                -- ^ title description download_url
                            |   WxppGetMaterialRaw ByteString LB.ByteString

instance FromJSON WxppGetMaterialResult where
    -- 这个函数不用处理 WxppGetMaterialRaw，因为这种情况并非用 JSON 表示
    parseJSON = withObject "WxppGetMaterialResult" $ \obj -> do
                    let parse_as_news = WxppGetMaterialNews <$> obj .: "news_item"
                        parse_as_video = WxppGetMaterialVideo
                                            <$> obj .: "title"
                                            <*> obj .: "description"
                                            <*> (UrlText <$> obj .: "down_url")
                    parse_as_news <|> parse_as_video


-- | 获取永久素材
wxppGetMaterial ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> WxppMaterialID
    -> m WxppGetMaterialResult
wxppGetMaterial (AccessToken { accessTokenData = atk }) (WxppMaterialID mid) = do
    let url = wxppRemoteApiBaseUrl <> "/material/get_material"
        opts = defaults & param "access_token" .~ [ atk ]
    r <- liftIO $ postWith opts url $ object [ "media_id" .= mid ]
    asWxppWsResponseNormal' r
        `catch`
        (\(_ :: JSONError) -> do
            return $ WxppGetMaterialRaw
                        (r ^. responseHeader "Content-Type")
                        (r ^. responseBody)
            )


-- | 永久素材统计数
data WxppMaterialCount = WxppMaterialCount {
                            wxppMaterialCountVoice      :: Int
                            , wxppMaterialCountVideo    :: Int
                            , wxppMaterialCountImage    :: Int
                            , wxppMaterialCountNews     :: Int
                        }

instance FromJSON WxppMaterialCount where
    parseJSON = withObject "WxppMaterialCount" $ \obj -> do
                    WxppMaterialCount
                        <$> ( obj .: "voice_count" )
                        <*> ( obj .: "video_count" )
                        <*> ( obj .: "image_count" )
                        <*> ( obj .: "news_count" )

instance ToJSON WxppMaterialCount where
    toJSON x = object
                [ "voice_count" .= wxppMaterialCountVoice x
                , "video_count" .= wxppMaterialCountVideo x
                , "image_count" .= wxppMaterialCountImage x
                , "news_count"  .= wxppMaterialCountNews x
                ]

wxppCountMaterial ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> m WxppMaterialCount
wxppCountMaterial (AccessToken { accessTokenData = atk }) = do
    let url = wxppRemoteApiBaseUrl <> "/material/get_materialcount"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ getWith opts url)
            >>= asWxppWsResponseNormal'


-- | 批量取永久素材接口，无论是图文还是一般的多媒体
-- 返回报文都是这样子的结构
data WxppBatchGetMaterialResult a = WxppBatchGetMaterialResult {
                                        wxppBatchGetMaterialResultTotal     :: Int
                                        , wxppBatchGetMaterialResultCount   :: Int
                                        , wxppBatchGetMaterialResultItems   :: [a]
                                    }

instance FromJSON a => FromJSON (WxppBatchGetMaterialResult a) where
    parseJSON = withObject "WxppBatchGetMaterialResult" $ \obj -> do
                    WxppBatchGetMaterialResult
                        <$> ( obj .: "total_count" )
                        <*> ( obj .: "item_count" )
                        <*> ( obj .: "item" )

-- | 批量取多媒体类型的永久素材，返回报文中的项目
data WxppBatchGetMaterialMediaItem = WxppBatchGetMaterialMediaItem {
                                        wxppBatchGetMaterialMediaItemID     :: WxppMaterialID
                                        , wxppBatchGetMaterialMediaItemName :: Text
                                        , wxppBatchGetMaterialMediaItemTime :: UTCTime
                                    }

instance FromJSON WxppBatchGetMaterialMediaItem where
    parseJSON = withObject "WxppBatchGetMaterialMediaItem" $ \obj -> do
                    WxppBatchGetMaterialMediaItem
                        <$> ( obj .: "media_id" )
                        <*> ( obj .: "name")
                        <*> ( epochIntToUtcTime <$> obj .: "update_time")

instance ToJSON WxppBatchGetMaterialMediaItem where
    toJSON x = object
                    [ "media_id" .= wxppBatchGetMaterialMediaItemID x
                    , "name" .= wxppBatchGetMaterialMediaItemName x
                    , "update_time" .= wxppBatchGetMaterialMediaItemTime x
                    ]

-- | 调用批量取多媒体类型的永久素材的接口
wxppBatchGetMaterialMedia ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> WxppMediaType
    -> Int      -- ^ limit
    -> Int      -- ^ offset
    -> m (WxppBatchGetMaterialResult WxppBatchGetMaterialMediaItem)
wxppBatchGetMaterialMedia (AccessToken { accessTokenData = atk }) mtype limit' offset' = do
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
        offset = max 1 $ offset'


-- | 批量取图文类型的永久素材，返回报文中的项目
data WxppBatchGetMaterialNewsItem = WxppBatchGetMaterialNewsItem {
                                        wxppBatchGetMaterialNewsItemID          :: WxppMaterialID
                                        , wxppBatchGetMaterialNewsItemContent   :: [WxppMaterialArticle]
                                        , wxppBatchGetMaterialNewsItemTime      :: UTCTime
                                    }
                                    deriving (Eq)

instance FromJSON WxppBatchGetMaterialNewsItem where
    parseJSON = withObject "WxppBatchGetMaterialNewsItem" $ \obj -> do
                    WxppBatchGetMaterialNewsItem
                        <$> ( obj .: "media_id" )
                        <*> ( obj .: "content" >>= (\o -> o .: "news_item"))
                        <*> ( epochIntToUtcTime <$> obj .: "update_time")

instance ToJSON WxppBatchGetMaterialNewsItem where
    toJSON x = object
                    [ "media_id" .= wxppBatchGetMaterialNewsItemID x
                    , "content" .= object [ "news_item" .= wxppBatchGetMaterialNewsItemContent x ]
                    , "update_time" .= utcTimeToEpochInt (wxppBatchGetMaterialNewsItemTime x)
                    ]

-- | 调用批量取图文消息类型的永久素材的接口
wxppBatchGetMaterialNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> Int      -- ^ limit
    -> Int      -- ^ offset
    -> m (WxppBatchGetMaterialResult WxppBatchGetMaterialNewsItem)
wxppBatchGetMaterialNews (AccessToken { accessTokenData = atk }) limit' offset' = do
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
        offset = max 1 $ offset'

-- | 把 limit/offset 风格的接口变成 conduit 风格：取全部数据
wxppBatchGetMaterialToSrc ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    (Int     -- ^ offset
        -> m (WxppBatchGetMaterialResult a)
    )   -- ^ 这个函数可以从前面的函数应用上部分参数后得到
    -> Source m a
wxppBatchGetMaterialToSrc get_by_offset = go 0
    where
        go offset = do
            result <- lift $ get_by_offset offset
            let items = wxppBatchGetMaterialResultItems result
            if null items
                then return ()
                else do
                    yieldMany items
                    let batch_count = wxppBatchGetMaterialResultCount result
                    go $ offset + batch_count


-- | 修改一个图文类型的永久素材: 只能修改其中一个文章
wxppReplaceArticleOfMaterialNews ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m ) =>
    AccessToken
    -> WxppMaterialID
    -> Int      -- ^ index
    -> WxppMaterialArticle
    -> m ()
wxppReplaceArticleOfMaterialNews (AccessToken { accessTokenData = atk }) material_id idx article = do
    let url = wxppRemoteApiBaseUrl <> "/material/update_news"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object $
                [ "media_id"    .= unWxppMaterialID material_id
                , "index"       .= idx
                , "articles"    .= article
                ])
    >>= asWxppWsResponseVoid
