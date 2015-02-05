module WeiXin.PublicPlatform.Media
    ( module WeiXin.PublicPlatform.Media
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Control.Monad.Logger
import Filesystem.Path.CurrentOS            (encodeString)
import Data.Acid                            (query)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.WS



-- | 上传本地一个文件
wxppUploadMedia ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> FilePath
    -> m UploadResult
wxppUploadMedia (AccessToken atk) mtype fp = do
    let type_s = case mtype of
                    WxppMediaTypeImage -> "image"
                    WxppMediaTypeVoice -> "voice"
                    WxppMediaTypeVideo -> "video"
                    WxppMediaTypeThumb -> "thumb"

    let url = wxppRemoteApiBaseUrl <> "/media/upload"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "type" .~ [ type_s :: Text]
    (liftIO $ postWith opts url $ partFileSource "media" $ encodeString fp)
            >>= asWxppWsResponseNormal'


wxppUploadMediaCached ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AcidState WxppAcidState
    -> AccessToken
    -> WxppMediaType
    -> FilePath
    -> m UploadResult
wxppUploadMediaCached acid atk mtype fp = do
    h <- liftIO $ md5HashFile fp
    m_res <- liftIO $ query acid $ WxppAcidLookupMediaIDByHash h
    now <- liftIO getCurrentTime
    maybe (wxppUploadMedia atk mtype fp) return $
        join $ flip fmap m_res $
                \x -> if (usableUploadResult now dt x && urMediaType x == mtype)
                            then Just x
                            else Nothing
    where
        dt = fromIntegral (60 * 60 * 24 :: Int)


-- | 把 WxppOutMsgL 里的文件路径变换成 WxppMediaID
fromWxppOutMsgL ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AcidState WxppAcidState
    -> AccessToken
    -> WxppOutMsgL
    -> m WxppOutMsg
fromWxppOutMsgL _       _   (WxppOutMsgTextL x)     = return (WxppOutMsgText x)
fromWxppOutMsgL _       _   (WxppOutMsgArticleL x)  = return (WxppOutMsgArticle x)
fromWxppOutMsgL acid    atk (WxppOutMsgImageL fp)   = do
        liftM (WxppOutMsgImage . urMediaId) $
            wxppUploadMediaCached acid atk WxppMediaTypeImage fp
fromWxppOutMsgL acid    atk (WxppOutMsgVoiceL fp)   = do
        liftM (WxppOutMsgVoice . urMediaId) $
            wxppUploadMediaCached acid atk WxppMediaTypeVoice fp
fromWxppOutMsgL acid    atk (WxppOutMsgVideoL fp x1 x2)   = do
        liftM ((\i -> WxppOutMsgVideo i x1 x2) . urMediaId) $
            wxppUploadMediaCached acid atk WxppMediaTypeVoice fp
fromWxppOutMsgL acid    atk (WxppOutMsgMusicL fp x1 x2 x3 x4)   = do
        liftM ((\i -> WxppOutMsgMusic i x1 x2 x3 x4) . urMediaId) $
            wxppUploadMediaCached acid atk WxppMediaTypeVoice fp

