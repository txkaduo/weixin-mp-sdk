module WeiXin.PublicPlatform.Class
    ( module WeiXin.PublicPlatform.Class
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Text.Shakespeare.I18N                (Lang)
import Data.Time                            (NominalDiffTime, diffUTCTime)
import Control.Monad.Trans.Maybe            (MaybeT(..))

import WeiXin.PublicPlatform.Types
import Data.List.NonEmpty                   as LNE


-- | 微信要求一些类似电子令牌、票据的数据尽量全局统一更新使用
-- 这接口描述的是更新这一类数据的更新接口
-- 更新这类数据，按微信的推荐，应由一个全局唯一的程序负责
class WxppCacheTokenUpdater a where
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

    wxppCacheAddJsTicket :: a
                        -> WxppAppID
                        -> WxppJsTicket
                        -> UTCTime
                        -> IO ()

    wxppCachePurgeJsTicket :: a
                            -> UTCTime
                            -> IO ()


-- | 与 WxppCacheTokenUpdater 相应的“读”接口
class WxppCacheTokenReader a where
    -- | get the lastest available access token
    wxppCacheGetAccessToken ::
        a
        -> WxppAppID
        -> IO (Maybe (AccessToken, UTCTime))
                -- ^ access toke and expiry time

    -- | lookup oauth access-token from cache
    wxppCacheGetOAuthAccessToken :: a
                                -> WxppAppID
                                -> WxppOpenID
                                -> Set OAuthScope
                                -> Text   -- ^ state
                                -> IO (Maybe OAuthTokenInfo)


    -- | 与 wxppCacheGetAccessToken, 如果 ticket 已过期，则返回 Nothing
    wxppCacheGetJsTicket :: a
                        -> WxppAppID
                        -> IO (Maybe (WxppJsTicket, UTCTime))


-- | 用户相关的信息的缓存接口
-- 这种数据来源于用户与系统的交互过程中
class WxppCacheTemp a where

    -- | add access-token of oauth to cache
    wxppCacheAddOAuthAccessToken :: a
                                -> OAuthAccessTokenPkg
                                -> UTCTime
                                -> IO ()

    wxppCachePurgeOAuthAccessToken :: a
                                    -> UTCTime
                                    -> IO ()

    wxppCacheAddSnsUserInfo :: a
                            -> WxppAppID
                            -> WxppOpenID
                            -> Lang
                            -> OAuthGetUserInfoResult
                            -> UTCTime
                            -> IO ()


    wxppCacheSaveUserInfo ::
        a
        -> WxppAppID
        -> EndUserQueryResult
        -> IO ()

    -- | User info from SNS api
    wxppCacheGetSnsUserInfo :: a
                            -> WxppAppID
                            -> WxppOpenID
                            -> Lang
                            -> IO (Maybe (OAuthGetUserInfoResult, UTCTime))

    wxppCacheLookupUserInfo ::
        a
        -> WxppAppID
        -> WxppOpenID
        -> IO (Maybe (EndUserQueryResult, UTCTime))


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


wxppGetUsableAccessToken :: (MonadIO m, WxppCacheTokenReader c) =>
    c -> WxppAppID -> m (Maybe (AccessToken, UTCTime))
wxppGetUsableAccessToken cache app_id = liftIO $ do
    now <- getCurrentTime
    fmap (join . (fmap $ \x -> if snd x > now then Just x else Nothing)) $
        wxppCacheGetAccessToken cache app_id

wxppGetSnsUserInfoCached :: (MonadIO m, WxppCacheTemp c)
                            => c
                            -> NominalDiffTime
                            -> WxppAppID
                            -> WxppOpenID
                            -> Lang
                            -> m (Maybe OAuthGetUserInfoResult)
wxppGetSnsUserInfoCached cache ttl app_id open_id lang = runMaybeT $ do
    (info, update_time) <- MaybeT $ liftIO $ wxppCacheGetSnsUserInfo cache app_id open_id lang
    now <- liftIO getCurrentTime
    guard $ diffUTCTime now update_time < ttl
    return info


data SomeWxppCacheBackend = forall a. (WxppCacheTokenReader a, WxppCacheTemp a) => SomeWxppCacheBackend a

instance WxppCacheTemp SomeWxppCacheBackend where
    wxppCacheAddSnsUserInfo (SomeWxppCacheBackend x)    = wxppCacheAddSnsUserInfo x
    wxppCacheSaveUserInfo   (SomeWxppCacheBackend x)    = wxppCacheSaveUserInfo x
    wxppCacheLookupUserInfo (SomeWxppCacheBackend x)    = wxppCacheLookupUserInfo x
    wxppCacheGetSnsUserInfo (SomeWxppCacheBackend x)  = wxppCacheGetSnsUserInfo x
    wxppCacheSaveUploadedMediaID
                            (SomeWxppCacheBackend x)    = wxppCacheSaveUploadedMediaID x
    wxppCacheLookupUploadedMediaIDByHash
                            (SomeWxppCacheBackend x)    = wxppCacheLookupUploadedMediaIDByHash x
    wxppCacheAddOAuthAccessToken (SomeWxppCacheBackend x) = wxppCacheAddOAuthAccessToken x
    wxppCachePurgeOAuthAccessToken (SomeWxppCacheBackend x) = wxppCachePurgeOAuthAccessToken x

instance WxppCacheTokenReader SomeWxppCacheBackend where
    wxppCacheGetAccessToken (SomeWxppCacheBackend x)    = wxppCacheGetAccessToken x
    wxppCacheGetOAuthAccessToken (SomeWxppCacheBackend x) = wxppCacheGetOAuthAccessToken x
    wxppCacheGetJsTicket (SomeWxppCacheBackend x)       = wxppCacheGetJsTicket x


data SomeWxppCacheTokenUpdater = forall a. (WxppCacheTokenUpdater a) => SomeWxppCacheTokenUpdater a

instance WxppCacheTokenUpdater SomeWxppCacheTokenUpdater where
    wxppCacheAddAccessToken (SomeWxppCacheTokenUpdater x)   = wxppCacheAddAccessToken x
    wxppCachePurgeAccessToken (SomeWxppCacheTokenUpdater x) = wxppCachePurgeAccessToken x
    wxppCacheAddJsTicket (SomeWxppCacheTokenUpdater x)      = wxppCacheAddJsTicket x
    wxppCachePurgeJsTicket (SomeWxppCacheTokenUpdater x)    = wxppCachePurgeJsTicket x


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
