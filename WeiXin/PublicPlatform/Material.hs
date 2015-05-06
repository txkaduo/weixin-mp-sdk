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
import Filesystem.Path.CurrentOS            (encodeString)
-- import Data.Acid                            (query)
import qualified Data.ByteString.Lazy       as LB
import Control.Monad.Catch                  (catch)
-- import Data.Yaml                            (ParseException)

import WeiXin.PublicPlatform.Types
-- import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.WS
-- import WeiXin.PublicPlatform.Utils


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
wxppUploadMaterialMediaInternal (AccessToken atk) mtype fp m_title_intro = do
    let type_s = case mtype of
                    WxppMediaTypeImage -> "image"
                    WxppMediaTypeVoice -> "voice"
                    WxppMediaTypeVideo -> "video"
                    WxppMediaTypeThumb -> "thumb"

    let url = wxppRemoteApiBaseUrl <> "/material/add_material"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $
                [ partFileSource "media" $ encodeString fp
                , partText "type" type_s
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
wxppUploadMaterialNews (AccessToken atk) news = do
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
wxppGetMaterial (AccessToken atk) (WxppMaterialID mid) = do
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
wxppCountMaterial (AccessToken atk) = do
    let url = wxppRemoteApiBaseUrl <> "/material/get_materialcount"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ getWith opts url)
            >>= asWxppWsResponseNormal'
