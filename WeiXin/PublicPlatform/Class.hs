{-# LANGUAGE DefaultSignatures #-}
module WeiXin.PublicPlatform.Class
    ( module WeiXin.PublicPlatform.Class
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Text.Shakespeare.I18N                (Lang)
import Data.Aeson                           (ToJSON(..), object, (.=))
import Data.Time                            (NominalDiffTime, diffUTCTime)
import Control.Monad.Trans.Maybe            (MaybeT(..))
import Crypto.Hash.TX.Utils                 (SHA256Hash(..))

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import Data.List.NonEmpty                   as LNE hiding (map, filter)


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
                            -> IO ()

    -- | User info from SNS api
    wxppCacheGetSnsUserInfo :: a
                            -> WxppAppID
                            -> WxppOpenID
                            -> Lang
                            -> IO (Maybe (OAuthGetUserInfoResult, UTCTime))
                            -- ^ user info and updated time


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


    -- | 由于微信没提供从union id找相关open id的接口
    -- 这个接口应尽一切可能, 根据union id找到所有已知的open id, 但不用保证结果是完整的
    wxppCacheLookupAllOpenIdByUid :: a
                                  -> WxppUnionID
                                  -> IO [WxppAppOpenID]

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


wxppCacheLookupOpenIdByUid :: MonadIO m
                           => WxppCacheTemp a
                           => a
                           -> WxppUnionID
                           -> WxppAppID
                           -> m (Maybe WxppOpenID)
wxppCacheLookupOpenIdByUid cache uid app_id = liftIO $ do
  fmap (fmap getWxppOpenID . listToMaybe . filter ((== app_id) . getWxppAppID)) $ wxppCacheLookupAllOpenIdByUid cache uid


-- | 用于实现中心缓存服务器
-- 此服务器主要责任是自动更新 access token 之类的数据
class WxppCacheAppRegistry a where
    wxppCacheRegistryAdd :: a
                            -> WxppAppID
                            -> WxppAppSecret
                            -> Maybe Token
                            -> IO ()

    wxppCacheRegistryGet :: a
                            -> WxppAppID
                            -> IO (Maybe (WxppAppSecret, Maybe Token))

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


-- | 有些接口调用需要用到 Secret，但又不想各个程序都配置一份secret
-- 可以由一个代理服务程序，代替其它程序去调用这种需要 Secret 的接口
class WxppApiBroker a where
  wxppApiBrokerOAuthGetAccessToken :: a
                                   -> WxppAppID
                                   -> OAuthCode
                                   -> IO (Maybe (WxppWsResp OAuthAccessTokenResult))
                                    -- ^ Nothing: if no such App


data SomeWxppApiBroker = forall a. WxppApiBroker a => SomeWxppApiBroker a

instance WxppApiBroker SomeWxppApiBroker where
  wxppApiBrokerOAuthGetAccessToken (SomeWxppApiBroker x) = wxppApiBrokerOAuthGetAccessToken x


class HasWxppApiBroker a where
  getWxppApiBroker :: a -> SomeWxppApiBroker

instance HasWxppApiBroker SomeWxppApiBroker where
  getWxppApiBroker = id


class HasAccessTokenIO a where
    wxppGetAccessTokenIO :: a -> IO (Maybe (AccessToken, UTCTime))
    default wxppGetAccessTokenIO :: HasAccessToken a => a -> IO (Maybe (AccessToken, UTCTime))
    wxppGetAccessTokenIO = return . Just . wxppGetAccessToken

class HasAccessTokenIO a => HasAccessToken a where
    wxppGetAccessToken :: a -> (AccessToken, UTCTime)

instance HasAccessToken (AccessToken, UTCTime) where
  wxppGetAccessToken = id

instance WxppCacheTokenReader a => HasAccessTokenIO (a, WxppAppID) where
  wxppGetAccessTokenIO = uncurry wxppCacheGetAccessToken

instance HasAccessTokenIO (AccessToken, UTCTime)

instance HasAccessTokenIO (IO (Maybe (AccessToken, UTCTime))) where
    wxppGetAccessTokenIO = id


wxppGetAccessTokenIOMay :: HasAccessTokenIO a
                        => a
                        -> IO (Maybe AccessToken)
wxppGetAccessTokenIOMay x = runMaybeT $ do
  (atk, expiry) <- MaybeT $ wxppGetAccessTokenIO x
  now <- liftIO getCurrentTime
  guard $ now < expiry
  return atk


-- | 微信用户的信息
-- 有两种来源，一是从已关注用户信息中取，另一种是oauth接口取任意授权用户的信息
data WxUserInfo = WxUserInfo
  { wxUserInfoOpenID     :: WxppOpenID
  , wxUserInfoNickname   :: Text
  , wxUserInfoGender     :: Maybe Gender
  , wxUserInfoCountry    :: Text
  , wxUserInfoProvince   :: Text
  , wxUserInfoCity       :: Text
  , wxUserInfoHeadImgUrl :: Maybe UrlText
  , wxUserInfoUnionID    :: Maybe WxppUnionID
  }
  deriving (Eq, Ord, Show)

instance ToJSON WxUserInfo where
  toJSON x = object [ "open_id" .= wxUserInfoOpenID x
                    , "nickname" .= wxUserInfoNickname x
                    , "gender" .= genderToInt (wxUserInfoGender x)
                    , "country" .= wxUserInfoCountry x
                    , "province" .= wxUserInfoProvince x
                    , "city" .= wxUserInfoCity x
                    , "head_img_url" .= wxUserInfoHeadImgUrl x
                    , "union_id" .= wxUserInfoUnionID x
                    ]

class MayHaveWxUserInfo a where
  mayGetWxUserInfo :: a -> Maybe WxUserInfo

class MayHaveWxUserInfo a => HasWxUserInfo a where
  getWxUserInfo :: a -> WxUserInfo

instance MayHaveWxUserInfo OAuthGetUserInfoResult where
  mayGetWxUserInfo = Just . getWxUserInfo

instance HasWxUserInfo OAuthGetUserInfoResult where
  getWxUserInfo x = WxUserInfo
                        (oauthUserInfoOpenID x)
                        (oauthUserInfoNickname x)
                        (oauthUserInfoGender x)
                        (oauthUserInfoCountry x)
                        (oauthUserInfoProvince x)
                        (oauthUserInfoCity x)
                        (oauthUserInfoHeadImgUrl x)
                        (oauthUserInfoUnionID x)

instance MayHaveWxUserInfo EndUserQueryResult where
  mayGetWxUserInfo (EndUserQueryResultNotSubscribed {}) = Nothing
  mayGetWxUserInfo
    (EndUserQueryResult open_id nickname m_gender _ city province country head_img_url _stime m_union_id)
    = Just $ WxUserInfo open_id nickname m_gender country province city head_img_url m_union_id



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

-- | Lookup SNS or subscribers user info
wxppGetAnyUserInfoCached :: (MonadIO m, WxppCacheTemp c)
                         => c
                         -> WxppAppID
                         -> WxppOpenID
                         -> Lang
                         -> m (Maybe (WxUserInfo, UTCTime))
wxppGetAnyUserInfoCached cache app_id open_id lang = do
  fmap (listToMaybe . ClassyPrelude.sortWith (Down . snd) . catMaybes) $ sequence [ runMaybeT by_sns, runMaybeT by_subs ]
  where
    by_sns = do
      fmap (first getWxUserInfo) $ MaybeT $ liftIO $ wxppCacheGetSnsUserInfo cache app_id open_id lang

    by_subs = do
      (m_info, t) <- fmap (first mayGetWxUserInfo) $ MaybeT $ liftIO $ wxppCacheLookupUserInfo cache app_id open_id
      info <- MaybeT $ return m_info
      return (info, t)


data SomeWxppCacheClient = forall a. (WxppCacheTokenReader a, WxppCacheTemp a) => SomeWxppCacheClient a

instance WxppCacheTemp SomeWxppCacheClient where
    wxppCacheAddSnsUserInfo (SomeWxppCacheClient x)        = wxppCacheAddSnsUserInfo x
    wxppCacheSaveUserInfo   (SomeWxppCacheClient x)        = wxppCacheSaveUserInfo x
    wxppCacheLookupUserInfo (SomeWxppCacheClient x)        = wxppCacheLookupUserInfo x
    wxppCacheLookupAllOpenIdByUid (SomeWxppCacheClient x)  = wxppCacheLookupAllOpenIdByUid x
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


class HasWxppCacheClient a where
  getWxppCacheClient :: a -> SomeWxppCacheClient

instance HasWxppCacheClient SomeWxppCacheClient where
  getWxppCacheClient = id


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
    wxppCacheLookupAllOpenIdByUid (SomeWxppCacheFull x)  = wxppCacheLookupAllOpenIdByUid x
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

instance HasWxppAppID WxppAppOpenID where
  getWxppAppID (WxppAppOpenID app_id _) = app_id

instance HasWxppAppID a => HasWxppAppID (a,b) where
    getWxppAppID = getWxppAppID . fst

class HasWxppOpenID a where
    getWxppOpenID :: a -> WxppOpenID

instance HasWxppOpenID WxppOpenID where
    getWxppOpenID = id

instance HasWxppOpenID WxppAppOpenID where
  getWxppOpenID (WxppAppOpenID _ oid) = oid

instance HasWxppOpenID EndUserQueryResult where
  getWxppOpenID = endUserQueryResultOpenID

instance HasWxppOpenID a => HasWxppOpenID (a,b) where
    getWxppOpenID = getWxppOpenID . fst

instance HasWxppAppID WxppAppConf where
  getWxppAppID = wxppConfAppID

instance HasWxppAppID WxppAppConfig where
  getWxppAppID = getWxppAppID . wxppConfigCore


class HasWxppSecret a where
  getWxppSecret :: a -> WxppAppSecret

instance HasWxppSecret WxppAppSecret where getWxppSecret = id

instance HasWxppSecret WxppAppConf where getWxppSecret = wxppConfAppSecret

instance HasWxppSecret WxppAppConfig where getWxppSecret = getWxppSecret . wxppConfigCore


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


class HasWxppToken a where
  getWxppToken :: a -> Token

instance HasWxppToken Token where getWxppToken = id

instance HasWxppToken WxppAppConf where
  getWxppToken = wxppConfAppToken

instance HasWxppToken WxppAppConfig where
  getWxppToken = getWxppToken . wxppConfigCore

class HasAesKeys a where
  getAesKeys :: a -> [AesKey]

instance HasAesKeys AesKey where getAesKeys = return

instance HasAesKeys [AesKey] where getAesKeys = id

instance HasAesKeys WxppAppConf where
  getAesKeys app_config = catMaybes $ wxppConfAppAesKey app_config :
                                        map Just (wxppConfAppBackupAesKeys app_config)

instance HasAesKeys WxppAppConfig where
  getAesKeys = getAesKeys . wxppConfigCore


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

    wxppCacheAddSnsUserInfo _ _ _ _ = return ()

    wxppCacheGetSnsUserInfo _ _ _ _ = return Nothing

    wxppCacheSaveUserInfo _ _ _ = return ()

    wxppCacheLookupUserInfo _ _ _ = return Nothing

    wxppCacheLookupAllOpenIdByUid _ _ = return []

    wxppCacheSaveUploadedMediaID _ _ _ _ = return ()

    wxppCacheLookupUploadedMediaIDByHash _ _ _ = return Nothing


instance WxppApiBroker FakeWxppCache where
    wxppApiBrokerOAuthGetAccessToken _ _ _ = return Nothing

instance WxppCacheAppRegistry FakeWxppCache where
    wxppCacheRegistryAdd _ _ _ _ = return ()

    wxppCacheRegistryGet _ _ = return Nothing

    wxppCacheRegistryDel _ _ = return ()

    wxppCacheRegistryDisable _ _ = return ()

    wxppCacheRegistryEnable _ _ = return ()
