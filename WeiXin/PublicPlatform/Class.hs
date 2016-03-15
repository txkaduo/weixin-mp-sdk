module WeiXin.PublicPlatform.Class
    ( module WeiXin.PublicPlatform.Class
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Text.Shakespeare.I18N                (Lang)
import Data.Time                            (NominalDiffTime, diffUTCTime)
import Control.Monad.Trans.Maybe            (MaybeT(..))
import Crypto.Hash.TX.Utils                 (SHA256Hash(..))

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


    -- | 与 wxppCacheGetAccessToken, 如果 ticket 已过期，则返回 Nothing
    wxppCacheGetJsTicket :: a
                        -> WxppAppID
                        -> IO (Maybe (WxppJsTicket, UTCTime))


-- | 用户相关的信息的缓存接口
-- 这种数据来源于用户与系统的交互过程中
class WxppCacheTemp a where
    -- | lookup oauth access-token from cache
    wxppCacheGetOAuthAccessToken :: a
                                -> WxppAppID
                                -> WxppOpenID
                                -> Set OAuthScope
                                -> IO (Maybe OAuthTokenInfo)


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
                            -> Lang
                            -> OAuthGetUserInfoResult
                            -> UTCTime
                            -> IO ()

    -- | User info from SNS api
    wxppCacheGetSnsUserInfo :: a
                            -> WxppAppID
                            -> WxppOpenID
                            -> Lang
                            -> IO (Maybe (OAuthGetUserInfoResult, UTCTime))


    wxppCacheSaveUserInfo ::
        a
        -> WxppAppID
        -> EndUserQueryResult
        -> IO ()

    wxppCacheLookupUserInfo ::
        a
        -> WxppAppID
        -> WxppOpenID
        -> IO (Maybe (EndUserQueryResult, UTCTime))


    wxppCacheSaveUploadedMediaID ::
        a
        -> WxppAppID
        -> SHA256Hash
        -> UploadResult
        -> IO ()

    wxppCacheLookupUploadedMediaIDByHash ::
        a
        -> WxppAppID
        -> SHA256Hash
        -> IO (Maybe UploadResult)


-- | 用于实现中心缓存服务器
-- 此服务器主要责任是自动更新 access token 之类的数据
class WxppCacheAppRegistry a where
    wxppCacheRegistryAdd :: a
                            -> WxppAppID
                            -> WxppAppSecret
                            -> Token
                            -> IO ()

    wxppCacheRegistryGet :: a
                            -> WxppAppID
                            -> IO (Maybe (WxppAppSecret, Token))

    wxppCacheRegistryDel :: a
                            -> WxppAppID
                            -> IO ()

    -- | 禁用的 App 不再会自动更新 access token 及其它 token
    -- 但已有数据还保留
    wxppCacheRegistryDisable :: a
                                -> WxppAppID
                                -> IO ()

    wxppCacheRegistryEnable :: a
                            -> WxppAppID
                            -> IO ()


data SomeWxppCacheAppRegistry = forall a. WxppCacheAppRegistry a => SomeWxppCacheAppRegistry a

instance WxppCacheAppRegistry SomeWxppCacheAppRegistry where
    wxppCacheRegistryAdd (SomeWxppCacheAppRegistry x)     = wxppCacheRegistryAdd x
    wxppCacheRegistryGet (SomeWxppCacheAppRegistry x)     = wxppCacheRegistryGet x
    wxppCacheRegistryDel (SomeWxppCacheAppRegistry x)     = wxppCacheRegistryDel x
    wxppCacheRegistryDisable (SomeWxppCacheAppRegistry x) = wxppCacheRegistryDisable x
    wxppCacheRegistryEnable (SomeWxppCacheAppRegistry x)  = wxppCacheRegistryEnable x


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


data SomeWxppCacheClient = forall a. (WxppCacheTokenReader a, WxppCacheTemp a) => SomeWxppCacheClient a

instance WxppCacheTemp SomeWxppCacheClient where
    wxppCacheAddSnsUserInfo (SomeWxppCacheClient x)        = wxppCacheAddSnsUserInfo x
    wxppCacheSaveUserInfo   (SomeWxppCacheClient x)        = wxppCacheSaveUserInfo x
    wxppCacheLookupUserInfo (SomeWxppCacheClient x)        = wxppCacheLookupUserInfo x
    wxppCacheGetSnsUserInfo (SomeWxppCacheClient x)        = wxppCacheGetSnsUserInfo x
    wxppCacheSaveUploadedMediaID
                            (SomeWxppCacheClient x)        = wxppCacheSaveUploadedMediaID x
    wxppCacheLookupUploadedMediaIDByHash
                            (SomeWxppCacheClient x)        = wxppCacheLookupUploadedMediaIDByHash x
    wxppCacheGetOAuthAccessToken (SomeWxppCacheClient x) = wxppCacheGetOAuthAccessToken x
    wxppCacheAddOAuthAccessToken (SomeWxppCacheClient x)   = wxppCacheAddOAuthAccessToken x
    wxppCachePurgeOAuthAccessToken (SomeWxppCacheClient x) = wxppCachePurgeOAuthAccessToken x

instance WxppCacheTokenReader SomeWxppCacheClient where
    wxppCacheGetAccessToken (SomeWxppCacheClient x)      = wxppCacheGetAccessToken x
    wxppCacheGetJsTicket (SomeWxppCacheClient x)         = wxppCacheGetJsTicket x


data SomeWxppCacheFull = forall a.
                                    ( WxppCacheTokenUpdater a
                                    , WxppCacheTokenReader a
                                    , WxppCacheTemp a
                                    )
                                    => SomeWxppCacheFull a

instance WxppCacheTokenUpdater SomeWxppCacheFull where
    wxppCacheAddAccessToken (SomeWxppCacheFull x)   = wxppCacheAddAccessToken x
    wxppCachePurgeAccessToken (SomeWxppCacheFull x) = wxppCachePurgeAccessToken x
    wxppCacheAddJsTicket (SomeWxppCacheFull x)      = wxppCacheAddJsTicket x
    wxppCachePurgeJsTicket (SomeWxppCacheFull x)    = wxppCachePurgeJsTicket x

instance WxppCacheTokenReader SomeWxppCacheFull where
    wxppCacheGetAccessToken (SomeWxppCacheFull x)      = wxppCacheGetAccessToken x
    wxppCacheGetJsTicket (SomeWxppCacheFull x)         = wxppCacheGetJsTicket x

instance WxppCacheTemp SomeWxppCacheFull where
    wxppCacheAddSnsUserInfo (SomeWxppCacheFull x)        = wxppCacheAddSnsUserInfo x
    wxppCacheSaveUserInfo   (SomeWxppCacheFull x)        = wxppCacheSaveUserInfo x
    wxppCacheLookupUserInfo (SomeWxppCacheFull x)        = wxppCacheLookupUserInfo x
    wxppCacheGetSnsUserInfo (SomeWxppCacheFull x)        = wxppCacheGetSnsUserInfo x
    wxppCacheSaveUploadedMediaID
                            (SomeWxppCacheFull x)        = wxppCacheSaveUploadedMediaID x
    wxppCacheLookupUploadedMediaIDByHash
                            (SomeWxppCacheFull x)        = wxppCacheLookupUploadedMediaIDByHash x
    wxppCacheGetOAuthAccessToken (SomeWxppCacheFull x) = wxppCacheGetOAuthAccessToken x
    wxppCacheAddOAuthAccessToken (SomeWxppCacheFull x)   = wxppCacheAddOAuthAccessToken x
    wxppCachePurgeOAuthAccessToken (SomeWxppCacheFull x) = wxppCachePurgeOAuthAccessToken x

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
    getSomeWxppCacheBackend :: a -> SomeWxppCacheClient

instance HasSomeWxppCacheBackend SomeWxppCacheClient where
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



-- | As a placeholder for testing
data FakeWxppCache = FakeWxppCache

instance WxppCacheTokenUpdater FakeWxppCache where
    -- | save a access token
    wxppCacheAddAccessToken _ _ _ = return ()

    wxppCachePurgeAccessToken _ _ = return ()

    wxppCacheAddJsTicket _ _ _ _ = return ()

    wxppCachePurgeJsTicket _ _ = return ()

instance WxppCacheTokenReader FakeWxppCache where
    wxppCacheGetAccessToken _ _ = return Nothing

    wxppCacheGetJsTicket _ _ = return Nothing


instance WxppCacheTemp FakeWxppCache where
    wxppCacheGetOAuthAccessToken _ _ _ _ = return Nothing

    wxppCacheAddOAuthAccessToken _ _ _ = return ()

    wxppCachePurgeOAuthAccessToken _ _ = return ()

    wxppCacheAddSnsUserInfo _ _ _ _ _ = return ()

    wxppCacheGetSnsUserInfo _ _ _ _ = return Nothing

    wxppCacheSaveUserInfo _ _ _ = return ()

    wxppCacheLookupUserInfo _ _ _ = return Nothing

    wxppCacheSaveUploadedMediaID _ _ _ _ = return ()

    wxppCacheLookupUploadedMediaIDByHash _ _ _ = return Nothing


instance WxppCacheAppRegistry FakeWxppCache where
    wxppCacheRegistryAdd _ _ _ _ = return ()

    wxppCacheRegistryGet _ _ = return Nothing

    wxppCacheRegistryDel _ _ = return ()

    wxppCacheRegistryDisable _ _ = return ()

    wxppCacheRegistryEnable _ _ = return ()
