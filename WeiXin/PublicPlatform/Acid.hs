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
import WeiXin.PublicPlatform.Types


-- | a helper used with 'WxppAcidGetAcccessTokens'
newestUsableAccessToken :: UTCTime -> [(AccessToken, UTCTime)] -> Maybe (AccessToken, UTCTime)
newestUsableAccessToken now lst =
    listToMaybe $ sortBy (comparing $ Down . snd) $ filter ((> now) . snd) $ lst


-- | 服务器进程需持久保持的数据
data WxppAcidState = WxppAcidState {
                    _wxppAcidStateAccessTokens :: ![(AccessToken, UTCTime)]
                        -- ^ access token 更新允许有个过滤期
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

wxppAcidLookupMediaIDByHash :: WxppAppID -> MD5Hash -> Query WxppAcidState (Maybe UploadResult)
wxppAcidLookupMediaIDByHash app_id h =
    asks $ preview $ wxppAcidStateUploadedMedia . at app_id . _Just . at h . _Just


-- | 为 EndUserQueryResult 增加缓存
wxppAcidSetCachedUserInfo ::
    UTCTime -> WxppOpenID -> WxppAppID -> EndUserQueryResult -> Update WxppAcidState ()
wxppAcidSetCachedUserInfo now open_id app_id qres = do
    modify $ over wxppAcidStateCachedUserInfo $
                Map.insert (open_id, app_id) (TimeTagged now qres)

wxppAcidGetCachedUserInfo ::
    WxppOpenID -> WxppAppID -> Query WxppAcidState (Maybe (TimeTagged EndUserQueryResult))
wxppAcidGetCachedUserInfo open_id app_id =
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
    , 'wxppAcidLookupMediaIDByHash
    , 'wxppAcidSetCachedUserInfo
    , 'wxppAcidGetCachedUserInfo
    , 'wxppAcidLookupCachedUnionID
    ])


wxppAcidGetUsableAccessToken :: MonadIO m =>
    AcidState WxppAcidState -> WxppAppID -> m (Maybe AccessToken)
wxppAcidGetUsableAccessToken acid app_id = liftIO $ do
    now <- getCurrentTime
    fmap (join . (fmap $ \x -> if snd x > now then Just (fst x) else Nothing)) $
        query acid $ WxppAcidGetAcccessToken app_id
