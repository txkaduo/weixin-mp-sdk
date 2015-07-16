module WeiXin.PublicPlatform.Acid
    ( module WeiXin.PublicPlatform.Acid
    , AcidState
    ) where

import ClassyPrelude
import Control.Lens
import Data.SafeCopy
import Data.Acid
import Data.Default                         (Default(..))
import Control.Monad.Reader                 (asks)
import Control.Monad.State                  (modify)
import qualified Data.Map.Strict            as Map

import Yesod.Helpers.SafeCopy
import WeiXin.PublicPlatform.Class

-- | a helper used with 'WxppAcidGetAcccessTokens'
newestUsableAccessToken :: UTCTime -> [(AccessToken, UTCTime)] -> Maybe (AccessToken, UTCTime)
newestUsableAccessToken now lst =
    listToMaybe $ sortBy (comparing $ Down . snd) $ filter ((> now) . snd) $ lst


-- | 服务器进程需持久保持的数据
data WxppAcidState = WxppAcidState {
                    _wxppAcidStateAccessTokens :: ![(AccessToken, UTCTime)]
                        -- ^ access token 更新允许有个过渡期
                        -- 因此记录了最近使用过的所有 AccessToken 及其过期时间
                        -- 这个列表实现时没有明确地限制长度
                        -- 而是定时丢弃过期的 access token
                        -- 注意：这个列表是排了序的
                    , _wxppAcidStateUploadedMedia :: !(Map WxppAppID (Map MD5Hash UploadResult))
                    , _wxppAcidStateCachedUserInfo :: !(Map (WxppOpenID, WxppAppID)
                                                            (TimeTagged EndUserQueryResult))
                        -- ^ 把用户的 EndUserQueryResult 缓存一下，以加快消息处理
                    }
                    deriving (Typeable)

$(makeLenses ''WxppAcidState)
$(deriveSafeCopy 0 'base ''WxppAcidState)

instance Default WxppAcidState where
    def = WxppAcidState def def def

wxppAcidGetAcccessTokens :: WxppAppID -> Query WxppAcidState [(AccessToken, UTCTime)]
wxppAcidGetAcccessTokens app_id =
    asks $ (filter $ (== app_id) . accessTokenApp . fst) . _wxppAcidStateAccessTokens

wxppAcidGetAcccessToken :: WxppAppID -> Query WxppAcidState (Maybe (AccessToken, UTCTime))
wxppAcidGetAcccessToken app_id =
    asks $ listToMaybe . (filter $ (== app_id) . accessTokenApp . fst) . _wxppAcidStateAccessTokens

wxppAcidAddAcccessToken ::
    AccessToken
    -> UTCTime
    -> Update WxppAcidState ()
wxppAcidAddAcccessToken atk expiry = do
    modify $ over wxppAcidStateAccessTokens $ \lst ->
        sortBy (comparing $ Down . snd) $ (atk, expiry) : lst

wxppAcidPurgeAcccessToken ::
    UTCTime -> Update WxppAcidState ()
wxppAcidPurgeAcccessToken expiry = do
    modify $ over wxppAcidStateAccessTokens $
                filter ((> expiry) . snd)

wxppAcidLookupUploadedMediaIDByHash ::
    WxppAppID -> MD5Hash -> Query WxppAcidState (Maybe UploadResult)
wxppAcidLookupUploadedMediaIDByHash app_id h =
    asks $ preview $ wxppAcidStateUploadedMedia . at app_id . _Just . at h . _Just

wxppAcidSaveUploadedMediaID ::
    WxppAppID
    -> MD5Hash
    -> UploadResult
    -> Update WxppAcidState ()
wxppAcidSaveUploadedMediaID app_id h u_res =
    modify $ over wxppAcidStateUploadedMedia $
        Map.unionWith Map.union (Map.singleton app_id (Map.singleton h u_res))


-- | 为 EndUserQueryResult 增加缓存
wxppAcidSetCachedUserInfo ::
    UTCTime -> WxppAppID -> WxppOpenID -> EndUserQueryResult -> Update WxppAcidState ()
wxppAcidSetCachedUserInfo now app_id open_id qres = do
    modify $ over wxppAcidStateCachedUserInfo $
                Map.insert (open_id, app_id) (TimeTagged now qres)

wxppAcidGetCachedUserInfo ::
    WxppAppID -> WxppOpenID -> Query WxppAcidState (Maybe (TimeTagged EndUserQueryResult))
wxppAcidGetCachedUserInfo app_id open_id =
    asks $ Map.lookup (open_id, app_id) .
            _wxppAcidStateCachedUserInfo

-- | 查找 UnionID 缓存
wxppAcidLookupCachedUnionID ::
    WxppOpenID -> WxppAppID -> Query WxppAcidState (Maybe (Maybe WxppUnionID, UTCTime))
wxppAcidLookupCachedUnionID open_id app_id =
    asks $ fmap ((endUserQueryResultUnionID . _unTimeTag) &&& _ttTime).
            Map.lookup (open_id, app_id) .
            _wxppAcidStateCachedUserInfo

$(makeAcidic ''WxppAcidState
    [ 'wxppAcidGetAcccessTokens
    , 'wxppAcidGetAcccessToken
    , 'wxppAcidAddAcccessToken
    , 'wxppAcidPurgeAcccessToken
    , 'wxppAcidLookupUploadedMediaIDByHash
    , 'wxppAcidSaveUploadedMediaID
    , 'wxppAcidSetCachedUserInfo
    , 'wxppAcidGetCachedUserInfo
    , 'wxppAcidLookupCachedUnionID
    ])


newtype WxppCacheByAcid = WxppCacheByAcid (AcidState WxppAcidState)

instance WxppCacheBackend WxppCacheByAcid where
    wxppCacheGetAccessToken (WxppCacheByAcid acid) app_id =
        query acid $ WxppAcidGetAcccessToken app_id

    wxppCacheAddAccessToken (WxppCacheByAcid acid) atk expiry = do
        update acid $ WxppAcidAddAcccessToken atk expiry

    wxppCachePurgeAccessToken (WxppCacheByAcid acid) expiry = do
        update acid $ WxppAcidPurgeAcccessToken expiry

    wxppCacheLookupUserInfo (WxppCacheByAcid acid) app_id open_id = do
        fmap (fmap $ _unTimeTag &&& _ttTime) $
            query acid $ WxppAcidGetCachedUserInfo app_id open_id

    wxppCacheSaveUserInfo (WxppCacheByAcid acid) app_id qres = do
        now <- liftIO getCurrentTime
        let open_id = endUserQueryResultOpenID qres
        update acid $ WxppAcidSetCachedUserInfo now app_id open_id qres

    wxppCacheLookupUploadedMediaIDByHash (WxppCacheByAcid acid) app_id h = do
        query acid $ WxppAcidLookupUploadedMediaIDByHash app_id h

    wxppCacheSaveUploadedMediaID (WxppCacheByAcid acid) app_id h ures = do
        update acid $ WxppAcidSaveUploadedMediaID app_id h ures
