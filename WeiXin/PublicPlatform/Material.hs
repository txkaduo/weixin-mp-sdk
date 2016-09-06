{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Material
    ( module WeiXin.PublicPlatform.Material
    , module WeiXin.PublicPlatform.Class
    ) where

import ClassyPrelude hiding (catch)
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Data.Aeson
import Control.Monad.Logger
import Control.Monad.Reader                 (asks)
import Data.SafeCopy
-- import Data.Acid                            (query)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Char8      as C8
import qualified Data.CaseInsensitive       as CI
import qualified Data.Text                  as T
import Control.Monad.Catch                  (catch)
import Network.Mime                         (MimeType)
-- import Data.Yaml                            (ParseException)
import Data.Conduit
import qualified Data.Conduit.List          as CL
-- import Data.Conduit.Combinators hiding (null)

import WeiXin.PublicPlatform.Class
-- import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Error
import WeiXin.PublicPlatform.Utils

-- | 查询永久素材中的图文消息中的文章，出现在从服务器返回的数据中
-- 与 WxppDurableArticle 几乎一样，区别只是
-- 多了 url 字段
data WxppDurableArticleS = WxppDurableArticleS {
                                wxppDurableArticleSA        :: WxppDurableArticle
                                , wxppDurableArticleSUrl    :: UrlText
                                }
                            deriving (Eq, Show)

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


-- | 根据已保存至素材库的图文消息，生成可回复用的图文消息
-- 比较麻烦的是封面图的URL
-- 这个URL可以比较曲折地取得：根据图文消息里的封面图media_id，遍历一次所有图片素材
-- 在media_id相同的项目内，找到其URL
-- (微信目前不提供根据media_id找到图片URL的直接接口）
wxppMakeArticleByDurableArticleS :: Maybe UrlText     -- ^ 封面图的URL
                                    -> WxppDurableArticleS
                                    -> WxppArticle
wxppMakeArticleByDurableArticleS m_pic_url s =
    WxppArticle
        (Just $ wxppDurableArticleTitle a)
        (wxppDurableArticleDigest a)
        m_pic_url
        (Just $ wxppDurableArticleSUrl s)
    where
        a = wxppDurableArticleSA s



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
wxppUploadDurableMediaInternal :: (WxppApiMonad env m)
                               => AccessToken
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
wxppUploadDurableMediaInternalLBS :: (WxppApiMonad env m)
                                  => AccessToken
                                  -> WxppMediaType
                                  -> Maybe (Text, Text)
                                      -- ^ 上传视频素材所需的额外信息：标题，简介
                                  -> Part                 -- ^ the @Part@ of uploaded file
                                  -> m DurableUploadResult
wxppUploadDurableMediaInternalLBS (AccessToken { accessTokenData = atk }) mtype m_title_intro fpart = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/material/add_material"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $
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
wxppUploadDurableMediaVideo :: (WxppApiMonad env m)
                            => AccessToken
                            -> Text     -- ^ 标题
                            -> Text     -- ^ 简介
                            -> FilePath
                            -> m DurableUploadResult
wxppUploadDurableMediaVideo atk title intro fp = do
    wxppUploadDurableMediaInternal atk WxppMediaTypeVideo (Just (title, intro)) fp


wxppUploadDurableMediaVideoLBS :: (WxppApiMonad env m)
                               => AccessToken
                               -> Text     -- ^ 标题
                               -> Text     -- ^ 简介
                               -> FilePath -- ^ a dummy file name
                               -> LB.ByteString
                               -> m DurableUploadResult
wxppUploadDurableMediaVideoLBS atk title intro fp lbs = do
    wxppUploadDurableMediaInternalLBS atk WxppMediaTypeVideo (Just (title, intro))
        (partLBS "media" lbs & partFileName .~ Just fp)


-- | 上传本地非视频媒体成为永久素材
wxppUploadDurableMediaNonVideo :: (WxppApiMonad env m)
                               => AccessToken
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
wxppUploadDurableMediaNonVideoLBS :: (WxppApiMonad env m)
                                  => AccessToken
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
wxppUploadDurableNews :: (WxppApiMonad env m)
                      => AccessToken
                      -> WxppDurableNews
                      -> m DurableUploadResult
wxppUploadDurableNews (AccessToken { accessTokenData = atk }) news = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/material/add_news"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ encode $ toJSON news)
            >>= asWxppWsResponseNormal'


-- | 查询永久素材的结果内容完全只能通过尝试的方式决定
-- 混合了多种情况
data WxppGetDurableResult =    WxppGetDurableNews [WxppDurableArticleS]
                            |   WxppGetDurableVideo Text Text UrlText
                                -- ^ title description download_url
                            |   WxppGetDurableRaw (Maybe MimeType) LB.ByteString
                            deriving (Show)

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
wxppGetDurableMedia :: (WxppApiMonad env m, MonadCatch m)
                    => AccessToken
                    -> WxppDurableMediaID
                    -> m WxppGetDurableResult
wxppGetDurableMedia (AccessToken { accessTokenData = atk }) (WxppDurableMediaID mid) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/material/get_material"
        opts = defaults & param "access_token" .~ [ atk ]

    r <- liftIO $ WS.postWith opts sess url $ object [ "media_id" .= mid ]
    asWxppWsResponseNormal' r
        `catch`
        (\(_ :: JSONError) -> do
            let resp_headers = r ^. responseHeaders
            $logDebugS wxppLogSource $ T.append "get_material api response headers:\n" $
                T.unlines $
                    flip map resp_headers $
                    \(ci_name, val) ->
                        fromString $ C8.unpack $ mconcat [ CI.original ci_name, ": ", val ]
            -- XXX: 微信平台返回的 Content-Type 总是 text/plain
            -- 要自己找 mime 内容
            let mime0   = r ^. responseHeader "Content-Type"
                m_mime  = if null mime0 || mime0 == "text/plain"
                           then Nothing
                           else Just mime0
            return $ WxppGetDurableRaw
                        m_mime
                        (r ^. responseBody)
            )

wxppGetDurableMediaMaybe :: (WxppApiMonad env m, MonadCatch m)
                         => AccessToken
                         -> WxppDurableMediaID
                         -> m (Maybe WxppGetDurableResult)
wxppGetDurableMediaMaybe atk mid = do
    (liftM Just $ wxppGetDurableMedia atk mid)
        `catch` \e -> do
            case e of
                WxppAppError xerr _
                    | wxppToErrorCodeX xerr == wxppToErrorCode WxppInvalidMediaId
                        -> return Nothing
                _ -> throwM e


-- | 组合了 wxppUploadDurableNews 与 wxppGetDurableMedia
-- 把 WxppDurableNews 保存至素材库后，再取出来，从而知道每个图文的URL
-- 再生成一个 WxppOutMsg
wxppUploadDurableNewsThenMakeOutMsg :: (WxppApiMonad env m, MonadCatch m)
                                    => AccessToken
                                    -> [(WxppDurableArticle, Maybe UrlText)]
                                    -> m (WxppDurableMediaID, WxppOutMsg)
wxppUploadDurableNewsThenMakeOutMsg atk article_and_url = do
    durable_media_id <- liftM durableUploadResultMediaID $
                            wxppUploadDurableNews atk $ WxppDurableNews $ map fst article_and_url
    get_result <- wxppGetDurableMedia atk durable_media_id
    case get_result of
        WxppGetDurableNews article_s_list -> do
            return $ (durable_media_id,) $ WxppOutMsgNews $
                flip map (zip article_s_list $ map snd article_and_url) $
                            \(article_s, m_pic_url) ->
                                wxppMakeArticleByDurableArticleS m_pic_url article_s

        _ -> do
            throwM $ userError $
                "wxppGetDurableMedia return unexpected result, should be a WxppGetDurableNews"
                <> ", but got " <> show get_result


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

wxppCountDurableMedia :: (WxppApiMonad env m)
                      => AccessToken
                      -> m WxppDurableCount
wxppCountDurableMedia (AccessToken { accessTokenData = atk }) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/material/get_materialcount"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.getWith opts sess url)
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
wxppBatchGetDurableMedia :: (WxppApiMonad env m)
                         => AccessToken
                         -> WxppMediaType
                         -> Int      -- ^ limit
                         -> Int      -- ^ offset
                         -> m (WxppBatchGetDurableResult WxppBatchGetDurableMediaItem)
wxppBatchGetDurableMedia (AccessToken { accessTokenData = atk }) mtype limit' offset' = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/material/batchget_material"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object $
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
wxppBatchGetDurableNews :: (WxppApiMonad env m)
                        => AccessToken
                        -> Int      -- ^ limit
                        -> Int      -- ^ offset
                        -> m (WxppBatchGetDurableResult WxppBatchGetDurableNewsItem)
wxppBatchGetDurableNews (AccessToken { accessTokenData = atk }) limit' offset' = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/material/batchget_material"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object $
                [ "type"    .= ("news" :: Text)
                , "count"   .= limit
                , "offset"  .= offset
                ])
            >>= asWxppWsResponseNormal'
    where
        limit = max 1 $ min 20 $ limit'
        offset = max 0 $ offset'

-- | 把 limit/offset 风格的接口变成 conduit 风格：取全部数据
wxppBatchGetDurableToSrc' :: (WxppApiMonad env m)
                          => (Int
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

wxppBatchGetDurableToSrc :: (WxppApiMonad env m)
                         => (Int
                              -> m (WxppBatchGetDurableResult a)
                         )    -- ^ 这个函数可以从前面的函数应用上部分参数后得到, Int 参数是 offset
                         -> Source m a
wxppBatchGetDurableToSrc get_by_offset =
    wxppBatchGetDurableToSrc' get_by_offset =$ CL.concatMap wxppBatchGetDurableResultItems

-- | 修改一个图文类型的永久素材: 只能修改其中一个文章
wxppReplaceArticleOfDurableNews :: (WxppApiMonad env m)
                                => AccessToken
                                -> WxppDurableMediaID
                                -> Int      -- ^ index
                                -> WxppDurableArticle
                                -> m ()
wxppReplaceArticleOfDurableNews (AccessToken { accessTokenData = atk }) material_id idx article = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/material/update_news"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object $
                [ "media_id"    .= unWxppDurableMediaID material_id
                , "index"       .= idx
                , "articles"    .= article
                ])
    >>= asWxppWsResponseVoid
