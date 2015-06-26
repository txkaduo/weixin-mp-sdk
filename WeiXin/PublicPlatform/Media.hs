{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Media
    ( module WeiXin.PublicPlatform.Media
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude hiding (FilePath, (<.>), (</>), catch)
import Network.Wreq
import Control.Lens
import Control.Monad.Logger
import Filesystem.Path.CurrentOS            (encodeString, FilePath)
import qualified Data.ByteString.Lazy       as LB
import Control.Monad.Catch                  (catch, catches, Handler(..))
import Data.Yaml                            (ParseException)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils


-- | 下载一个多媒体文件
wxppDownloadMedia ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    AccessToken
    -> WxppBriefMediaID
    -> m (Response LB.ByteString)
wxppDownloadMedia (AccessToken { accessTokenData = atk }) mid = do
    let url = wxppRemoteApiBaseUrl <> "/media/get"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "media_id" .~ [ unWxppBriefMediaID mid ]
    rb <- liftIO $ getWith opts url

    -- 需要某种方法区别正常和异常的返回
    -- 使用 Content-Type 的方式
    -- 但 content-type 的内容严格地说不是一个简单的字串比较就了事的
    -- rb ^. responseHeader "Content-Type"
    -- 这里使用的方法是先测试一下当作错误报告的 json 解释，不行就认为是正常返回
    let as_json = liftM (view responseBody) $ asJSON $ alterContentTypeToJson rb
        unexpected_jsn = \(_ :: ()) -> do
            -- 至此，说明报文真的是个json，而且不是错误报文
            $logErrorS wxppLogSource $ "cannot parse download media respnose."
            throwM $ userError "cannot parse download media respnose."

    (as_json >>= either throwM unexpected_jsn . unWxppWsResp)
                `catch`
                    (\(_ :: JSONError) -> return rb)



-- | 上传本地一个文件
wxppUploadMedia ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> WxppMediaType
    -> FilePath
    -> m UploadResult
wxppUploadMedia (AccessToken { accessTokenData = atk }) mtype fp = do
    let url = wxppRemoteApiBaseUrl <> "/media/upload"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "type" .~ [ wxppMediaTypeString mtype :: Text]
    (liftIO $ postWith opts url $ partFileSource "media" $ encodeString fp)
            >>= asWxppWsResponseNormal'


wxppUploadMediaCached ::
    ( MonadIO m, MonadLogger m, MonadThrow m, WxppCacheBackend c) =>
    c
    -> AccessToken
    -> WxppMediaType
    -> FilePath
    -> m UploadResult
wxppUploadMediaCached cache atk mtype fp = do
    h <- liftIO $ md5HashFile fp
    m_res <- liftIO $ wxppCacheLookupUploadedMediaIDByHash cache app_id h
    now <- liftIO getCurrentTime
    u_res <- maybe (wxppUploadMedia atk mtype fp) return $
        join $ flip fmap m_res $
                \x -> if (usableUploadResult now dt x && urMediaType x == mtype)
                            then Just x
                            else Nothing

    liftIO $ wxppCacheSaveUploadedMediaID cache app_id h u_res
    return u_res
    where
        dt = fromIntegral (60 * 60 * 24 :: Int)
        app_id = accessTokenApp atk


-- | 从 WxppOutMsgL 计算出 WxppOutMsg
-- 工作包括：
-- * 把 WxppOutMsgL 里的文件路径变换成 WxppBriefMediaID
-- * 执行必要的延迟加载
-- 这个函数会抛出异常 见 tryWxppWsResult
-- 下面还有个尽量不抛出异常的版本
fromWxppOutMsgL ::
    ( MonadIO m, MonadLogger m, MonadThrow m, WxppCacheBackend c) =>
    FilePath
    -> c
    -> AccessToken
    -> WxppOutMsgL
    -> m WxppOutMsg
fromWxppOutMsgL _       _   _   (WxppOutMsgTextL x)     = return (WxppOutMsgText x)

fromWxppOutMsgL msg_dir _   _   (WxppOutMsgNewsL loaders)  =
        liftM WxppOutMsgNews $ do
            sequence $ map (runDelayedYamlLoaderExc msg_dir) loaders

fromWxppOutMsgL _ cache    atk (WxppOutMsgImageL fp)   = do
        liftM (WxppOutMsgImage . urMediaId) $
            wxppUploadMediaCached cache atk WxppMediaTypeImage fp

fromWxppOutMsgL _ cache    atk (WxppOutMsgVoiceL fp)   = do
        liftM (WxppOutMsgVoice . urMediaId) $
            wxppUploadMediaCached cache atk WxppMediaTypeVoice fp

fromWxppOutMsgL _ cache    atk (WxppOutMsgVideoL fp m_fp2 x1 x2)   = do
        liftM2 (\i i2 -> WxppOutMsgVideo i i2 x1 x2)
            (liftM urMediaId $ wxppUploadMediaCached cache atk WxppMediaTypeVoice fp)
            (liftM (fmap urMediaId) $ mapM (wxppUploadMediaCached cache atk WxppMediaTypeVoice) m_fp2)

fromWxppOutMsgL _ cache    atk (WxppOutMsgMusicL fp x1 x2 x3 x4)   = do
        liftM ((\i -> WxppOutMsgMusic i x1 x2 x3 x4) . urMediaId) $
            wxppUploadMediaCached cache atk WxppMediaTypeVoice fp

fromWxppOutMsgL _ _       _   WxppOutMsgTransferToCustomerServiceL =
                                                    return WxppOutMsgTransferToCustomerService


fromWxppOutMsgL' ::
    ( MonadIO m, MonadLogger m, MonadCatch m, WxppCacheBackend c) =>
    FilePath
    -> c
    -> AccessToken
    -> WxppOutMsgL
    -> m (Either String WxppOutMsg)
fromWxppOutMsgL' fp cache atk out_msg_l =
    (liftM Right $ fromWxppOutMsgL fp cache atk out_msg_l) `catches`
        (Handler h_yaml_exc : map unifyExcHandler wxppWsExcHandlers)
    where
        h_yaml_exc e = return $ Left $ show (e :: ParseException)
