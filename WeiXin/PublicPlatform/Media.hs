{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Media
    ( module WeiXin.PublicPlatform.Media
    , module WeiXin.PublicPlatform.Class
    ) where

import ClassyPrelude hiding (catch)
import qualified Data.Text                  as T
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader                 (asks)
import Network.Mime                         (MimeType, defaultMimeLookup)
import qualified Data.ByteString.Lazy       as LB
import Control.Monad.Catch                  (catch, catches, Handler(..))
import Data.Yaml                            (ParseException)
import Data.List.NonEmpty                   as LNE
import Data.Aeson                           (FromJSON(..), withObject, (.:))
import Network.HTTP                         (urlEncodeVars)
import System.FilePath                      (takeFileName)
import Crypto.Hash.TX.Utils                 (sha256HashFile, sha256HashBS)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils


-- | 生成直接下载临时素材的URL，可用于从第三方平台直接下载
-- CAUTION: URL 包含 access token 敏感信息，不要交给不可信的第三方
wxppDownloadMediaUrl :: WxppUrlConfig
                     -> Bool    -- ^ 文档说下载视频时不能用 https，但这个信息只有调用者才知道
                     -> AccessToken
                     -> WxppBriefMediaID
                     -> UrlText
wxppDownloadMediaUrl url_conf if_ssl (AccessToken { accessTokenData = atk }) mid =
    UrlText $ fromString $ url0 <> "?" <> qs
    where
        url0 = (if if_ssl then wxppUrlConfSecureApiBase else wxppUrlConfNonSecureApiBase) url_conf
                <> "/media/get"
        vars =  [ ("access_token", T.unpack atk)
                , ("media_id", T.unpack (unWxppBriefMediaID mid))
                ]
        qs  = urlEncodeVars vars


-- | 下载一个多媒体文件
wxppDownloadMedia :: ( WxppApiMonad env m, MonadCatch m )
                  => Bool    -- ^ 文档说下载视频时不能用 https，但这个信息只有调用者才知道
                  -> AccessToken
                  -> WxppBriefMediaID
                  -> m (Response LB.ByteString)
wxppDownloadMedia if_ssl (AccessToken { accessTokenData = atk }) mid = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = (if if_ssl then wxppUrlConfSecureApiBase else wxppUrlConfNonSecureApiBase) url_conf
                <> "/media/get"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "media_id" .~ [ unWxppBriefMediaID mid ]

    rb <- liftIO $ WS.getWith opts sess url

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


wxppUploadMediaInternal :: ( WxppApiMonad env m )
                        => AccessToken
                        -> WxppMediaType
                        -> Part
                        -> m UploadResult
wxppUploadMediaInternal (AccessToken { accessTokenData = atk }) mtype fpart = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/media/upload"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "type" .~ [ wxppMediaTypeString mtype :: Text]

    liftIO (WS.postWith opts sess  url $ fpart & partName .~ "media")
        >>= asWxppWsResponseNormal'


-- | 上传本地一个文件
wxppUploadMedia :: ( WxppApiMonad env m )
                => AccessToken
                -> WxppMediaType
                -> FilePath
                -> m UploadResult
wxppUploadMedia atk mtype fp = do
    wxppUploadMediaInternal atk mtype $
        partFileSource "media" fp
            & partContentType .~ Just (defaultMimeLookup $ fromString $ takeFileName fp)

-- | 上传已在内存中的 ByteString
wxppUploadMediaBS :: ( WxppApiMonad env m )
                  => AccessToken
                  -> WxppMediaType
                  -> MimeType
                  -> String
                  -> ByteString
                  -> m UploadResult
wxppUploadMediaBS atk mtype mime filename bs = do
    wxppUploadMediaInternal atk mtype $
        partBS "media" bs & partFileName .~ Just filename
                            & partContentType .~ Just mime

wxppUploadMediaLBS :: (WxppApiMonad env m)
                   => AccessToken
                   -> WxppMediaType
                   -> MimeType
                   -> String
                   -> LB.ByteString
                   -> m UploadResult
wxppUploadMediaLBS atk mtype mime filename bs = do
    wxppUploadMediaInternal atk mtype $
        partLBS "media" bs & partFileName .~ Just filename
                            & partContentType .~ Just mime

wxppUploadMediaCached :: ( WxppApiMonad env m, WxppCacheTokenReader c, WxppCacheTemp c)
                      => c
                      -> AccessToken
                      -> WxppMediaType
                      -> FilePath
                      -> m UploadResult
wxppUploadMediaCached cache atk mtype fp = do
    h <- liftIO $ sha256HashFile fp
    m_res <- liftIO $ wxppCacheLookupUploadedMediaIDByHash cache app_id h
    now <- liftIO getCurrentTime
    let m_res' = join $ flip fmap m_res $
                    \x -> if (usableUploadResult now dt x && urMediaType x == mtype)
                                then Just x
                                else Nothing
    case m_res' of
        Nothing -> do
            u_res <- wxppUploadMedia atk mtype fp
            liftIO $ wxppCacheSaveUploadedMediaID cache app_id h u_res
            return u_res

        Just x -> return x

    where
        dt = fromIntegral (60 * 60 * 24 :: Int)
        app_id = accessTokenApp atk


-- | 这里有个问题：如果之前上传过的文件的 mime 发生变化，可能会使用旧的文件 media id
wxppUploadMediaCachedBS :: ( WxppApiMonad env m, WxppCacheTokenReader c, WxppCacheTemp c )
                        => c
                        -> AccessToken
                        -> WxppMediaType
                        -> MimeType
                        -> String
                        -> ByteString
                        -> m UploadResult
wxppUploadMediaCachedBS cache atk mtype mime filename bs = do
    let h = sha256HashBS bs
    m_res <- liftIO $ wxppCacheLookupUploadedMediaIDByHash cache app_id h
    now <- liftIO getCurrentTime
    u_res <- maybe (wxppUploadMediaBS atk mtype mime filename bs) return $
        join $ flip fmap m_res $
                \x -> if (usableUploadResult now dt x && urMediaType x == mtype)
                            then Just x
                            else Nothing

    liftIO $ wxppCacheSaveUploadedMediaID cache app_id h u_res
    return u_res
    where
        dt = fromIntegral (60 * 60 * 24 :: Int)
        app_id = accessTokenApp atk


newtype UploadImgResult = UploadImgResult UrlText

instance FromJSON UploadImgResult where
    parseJSON = withObject "UploadImgResult" $ \o ->
                    UploadImgResult <$> o .: "url"

-- | 这是在群发文档中描述的特别接口，只用于上传图片，jpg/png格式
-- 不占用永久素材限额
-- 回复得到一个 URL，用于图文消息中
wxppUploadImageGetUrlInternal :: ( WxppApiMonad env m )
                              => AccessToken
                              -> Part
                              -> m UrlText
wxppUploadImageGetUrlInternal (AccessToken { accessTokenData = atk }) fpart = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/media/uploadimg"
        opts = defaults & param "access_token" .~ [ atk ]

    UploadImgResult media_url <- liftIO (WS.postWith opts sess url $ fpart & partName .~ "media")
        >>= asWxppWsResponseNormal'
    return media_url

wxppUploadImageGetUrl :: (WxppApiMonad env m)
                      => AccessToken
                      -> FilePath
                      -> m UrlText
wxppUploadImageGetUrl atk fp = do
    wxppUploadImageGetUrlInternal atk $ partFileSource "media" fp

wxppUploadImageGetUrlBS :: (WxppApiMonad env m)
                        => AccessToken
                        -> MimeType
                        -> FilePath
                        -> ByteString
                        -> m UrlText
wxppUploadImageGetUrlBS atk mime filename bs = do
    wxppUploadImageGetUrlInternal atk $
        partBS "media" bs & partFileName .~ Just filename
                            & partContentType .~ Just mime


-- | 从 WxppOutMsgL 计算出 WxppOutMsg
-- 工作包括：
-- * 把 WxppOutMsgL 里的文件路径变换成 WxppBriefMediaID
-- * 执行必要的延迟加载
-- 这个函数会抛出异常 见 tryWxppWsResult
-- 下面还有个尽量不抛出异常的版本
fromWxppOutMsgL :: (WxppApiMonad env m, WxppCacheTokenReader c, WxppCacheTemp c)
                => NonEmpty FilePath
                -> c
                -> m AccessToken
                -> WxppOutMsgL
                -> m WxppOutMsg
fromWxppOutMsgL _       _   _   (WxppOutMsgTextL x)     = return (WxppOutMsgText x)

fromWxppOutMsgL msg_dir _   _   (WxppOutMsgNewsL loaders)  =
        liftM WxppOutMsgNews $ do
            sequence $ fmap (runDelayedYamlLoaderExcL msg_dir) loaders

fromWxppOutMsgL _ cache    get_atk (WxppOutMsgImageL (Right fp))   = do
        atk <- get_atk
        liftM (WxppOutMsgImage . fromWxppBriefMediaID . urMediaId) $
            wxppUploadMediaCached cache atk WxppMediaTypeImage fp

fromWxppOutMsgL _ _cache    _get_atk (WxppOutMsgImageL (Left media_id))   = do
        return $ WxppOutMsgImage media_id

fromWxppOutMsgL _ cache    get_atk (WxppOutMsgVoiceL (Right fp))   = do
        atk <- get_atk
        liftM (WxppOutMsgVoice . fromWxppBriefMediaID . urMediaId) $
            wxppUploadMediaCached cache atk WxppMediaTypeVoice fp

fromWxppOutMsgL _ _cache    _get_atk (WxppOutMsgVoiceL (Left media_id))   = do
        return $ WxppOutMsgVoice  media_id

fromWxppOutMsgL _ _cache    _get_atk (WxppOutMsgVideoL (Left media_id) Nothing x1 x2)   = do
        return $ WxppOutMsgVideo media_id Nothing x1 x2

fromWxppOutMsgL _ _cache    _get_atk (WxppOutMsgVideoL (Left media_id) (Just (Left thumb_mid)) x1 x2)   = do
        return $ WxppOutMsgVideo media_id (Just thumb_mid) x1 x2

fromWxppOutMsgL _ cache    get_atk (WxppOutMsgVideoL mp m_mp2 x1 x2)   = do
        atk <- get_atk
        media_id <- either
                    return
                    (\fp -> liftM (fromWxppBriefMediaID . urMediaId) $
                                wxppUploadMediaCached cache atk WxppMediaTypeVideo fp)
                    mp
        thumb_mid <- forM m_mp2 $
                        either
                            return
                            (\fp -> liftM (fromWxppBriefMediaID . urMediaId) $
                                        wxppUploadMediaCached cache atk WxppMediaTypeImage fp)

        return $ WxppOutMsgVideo media_id thumb_mid x1 x2

fromWxppOutMsgL _ cache    get_atk (WxppOutMsgMusicL (Right fp) x1 x2 x3 x4)   = do
        atk <- get_atk
        liftM ((\i -> WxppOutMsgMusic i x1 x2 x3 x4) . fromWxppBriefMediaID . urMediaId) $
            wxppUploadMediaCached cache atk WxppMediaTypeImage fp

fromWxppOutMsgL _ _cache    _get_atk (WxppOutMsgMusicL (Left media_id) x1 x2 x3 x4)   = do
        return $ WxppOutMsgMusic media_id x1 x2 x3 x4

fromWxppOutMsgL _ _       _   WxppOutMsgTransferToCustomerServiceL =
                                                    return WxppOutMsgTransferToCustomerService


fromWxppOutMsgL' :: (WxppApiMonad env m, MonadCatch m, WxppCacheTokenReader c, WxppCacheTemp c)
                 => NonEmpty FilePath
                 -> c
                 -> m AccessToken
                 -> WxppOutMsgL
                 -> m (Either String WxppOutMsg)
fromWxppOutMsgL' fp cache get_atk out_msg_l =
    (liftM Right $ fromWxppOutMsgL fp cache get_atk out_msg_l) `catches`
        (Handler h_yaml_exc : fmap unifyExcHandler wxppWsExcHandlers)
    where
        h_yaml_exc e = return $ Left $ show (e :: ParseException)
