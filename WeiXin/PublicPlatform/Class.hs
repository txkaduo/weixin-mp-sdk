module WeiXin.PublicPlatform.Class
    ( module WeiXin.PublicPlatform.Class
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude

import WeiXin.PublicPlatform.Types
import Data.List.NonEmpty                   as LNE


-- | WXPP 服务器所需的一切 cache 接口
-- 实际cache可以用各种后端，包括acid-state，各种数据库等等
class WxppCacheBackend a where
    -- | get the lastest available access token
    wxppCacheGetAccessToken ::
        a
        -> WxppAppID
        -> IO (Maybe (AccessToken, UTCTime))
                -- ^ access toke and expiry time

    -- | save a access token
    wxppCacheAddAccessToken ::
        a
        -> AccessToken
        -> UTCTime      -- ^ expiry time
        -> IO ()

    wxppCachePurgeAccessToken ::
        a
        -> UTCTime      -- ^ expiry time
        -> IO ()

    -- | add access-token of oauth to cache
    wxppCacheAddOAuthAccessToken :: a
                                -> OAuthAccessTokenPkg
                                -> UTCTime
                                -> IO ()

    -- | lookup oauth access-token from cache
    wxppCacheGetOAuthAccessToken :: a
                                -> WxppAppID
                                -> WxppOpenID
                                -> Set OAuthScope
                                -> Text   -- ^ state
                                -> IO (Maybe OAuthTokenInfo)

    wxppCachePurgeOAuthAccessToken :: a
                                    -> UTCTime
                                    -> IO ()

    wxppCacheLookupUserInfo ::
        a
        -> WxppAppID
        -> WxppOpenID
        -> IO (Maybe (EndUserQueryResult, UTCTime))

    wxppCacheSaveUserInfo ::
        a
        -> WxppAppID
        -> EndUserQueryResult
        -> IO ()

    wxppCacheLookupUploadedMediaIDByHash ::
        a
        -> WxppAppID
        -> SHA256Hash
        -> IO (Maybe UploadResult)

    wxppCacheSaveUploadedMediaID ::
        a
        -> WxppAppID
        -> SHA256Hash
        -> UploadResult
        -> IO ()


class HasAccessToken a where
    wxppGetAccessToken :: a -> IO (Maybe (AccessToken, UTCTime))

instance HasAccessToken a => HasAccessToken (a, b) where
    wxppGetAccessToken = wxppGetAccessToken . fst

instance HasAccessToken (IO (Maybe (AccessToken, UTCTime))) where
    wxppGetAccessToken = id


wxppGetUsableAccessToken :: (MonadIO m, WxppCacheBackend c) =>
    c -> WxppAppID -> m (Maybe (AccessToken, UTCTime))
wxppGetUsableAccessToken cache app_id = liftIO $ do
    now <- getCurrentTime
    fmap (join . (fmap $ \x -> if snd x > now then Just x else Nothing)) $
        wxppCacheGetAccessToken cache app_id


data SomeWxppCacheBackend = forall a. WxppCacheBackend a => SomeWxppCacheBackend a

instance WxppCacheBackend SomeWxppCacheBackend where
    wxppCacheGetAccessToken (SomeWxppCacheBackend x)    = wxppCacheGetAccessToken x
    wxppCacheAddAccessToken (SomeWxppCacheBackend x)    = wxppCacheAddAccessToken x
    wxppCachePurgeAccessToken (SomeWxppCacheBackend x)  = wxppCachePurgeAccessToken x
    wxppCacheAddOAuthAccessToken (SomeWxppCacheBackend x) = wxppCacheAddOAuthAccessToken x
    wxppCachePurgeOAuthAccessToken (SomeWxppCacheBackend x) = wxppCachePurgeOAuthAccessToken x
    wxppCacheGetOAuthAccessToken (SomeWxppCacheBackend x) = wxppCacheGetOAuthAccessToken x
    wxppCacheLookupUserInfo (SomeWxppCacheBackend x)    = wxppCacheLookupUserInfo x
    wxppCacheSaveUserInfo   (SomeWxppCacheBackend x)    = wxppCacheSaveUserInfo x
    wxppCacheLookupUploadedMediaIDByHash
                            (SomeWxppCacheBackend x)    = wxppCacheLookupUploadedMediaIDByHash x
    wxppCacheSaveUploadedMediaID
                            (SomeWxppCacheBackend x)    = wxppCacheSaveUploadedMediaID x


class HasWxppAppID a where
    getWxppAppID :: a -> WxppAppID

instance HasWxppAppID WxppAppID where
    getWxppAppID = id

instance HasWxppAppID a => HasWxppAppID (a,b) where
    getWxppAppID = getWxppAppID . fst

class HasWxppOpenID a where
    getWxppOpenID :: a -> WxppOpenID

instance HasWxppOpenID WxppOpenID where
    getWxppOpenID = id

instance HasWxppOpenID a => HasWxppOpenID (a,b) where
    getWxppOpenID = getWxppOpenID . fst


class HasSomeWxppCacheBackend a where
    getSomeWxppCacheBackend :: a -> SomeWxppCacheBackend

instance HasSomeWxppCacheBackend SomeWxppCacheBackend where
    getSomeWxppCacheBackend = id

instance HasSomeWxppCacheBackend a => HasSomeWxppCacheBackend (a,b) where
    getSomeWxppCacheBackend = getSomeWxppCacheBackend . fst


class HasWxppOutMsgDir a where
    getWxppOutMsgDir :: a -> NonEmpty FilePath

instance HasWxppOutMsgDir a => HasWxppOutMsgDir (a,b) where
    getWxppOutMsgDir = getWxppOutMsgDir . fst

instance HasWxppOutMsgDir (NonEmpty FilePath) where
    getWxppOutMsgDir = id

instance HasWxppOutMsgDir FilePath where
    getWxppOutMsgDir x = x :| []
