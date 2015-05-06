{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Material
    ( module WeiXin.PublicPlatform.Material
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Data.Aeson
import Control.Monad.Logger
import Filesystem.Path.CurrentOS            (encodeString)
-- import Data.Acid                            (query)
-- import qualified Data.ByteString.Lazy       as LB
-- import Control.Monad.Catch                  (catches, Handler(..))
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

    let url = wxppRemoteFileApiBaseUrl <> "/material/add_material"
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
    let url = wxppRemoteFileApiBaseUrl <> "/material/add_news"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ encode $ toJSON news)
            >>= asWxppWsResponseNormal'
