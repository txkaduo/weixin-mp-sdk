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
                    , _wxppAcidStateUploadedMedia :: !(Map MD5Hash UploadResult)
                    , _wxppAcidStateCachedUnionID :: !(Map (WxppOpenID, WxppAppID) (WxppUnionID, UTCTime))
                        -- ^ 把用户的 UnionID 缓存一下，以加快消息处理
                    }
                    deriving (Typeable)

$(makeLenses ''WxppAcidState)
$(deriveSafeCopy 0 'base ''WxppAcidState)

instance Default WxppAcidState where
    def = WxppAcidState def def def

wxppAcidGetAcccessTokens :: Query WxppAcidState [(AccessToken, UTCTime)]
wxppAcidGetAcccessTokens =
    asks _wxppAcidStateAccessTokens

wxppAcidGetAcccessToken :: Query WxppAcidState (Maybe (AccessToken, UTCTime))
wxppAcidGetAcccessToken =
    asks $ listToMaybe . _wxppAcidStateAccessTokens

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

wxppAcidLookupMediaIDByHash :: MD5Hash -> Query WxppAcidState (Maybe UploadResult)
wxppAcidLookupMediaIDByHash h =
    asks $ view $ wxppAcidStateUploadedMedia . at h


-- | 为 UnionID 增加缓存
wxppAcidSetCachedUnionID ::
    UTCTime -> WxppOpenID -> WxppAppID -> WxppUnionID -> Update WxppAcidState ()
wxppAcidSetCachedUnionID now open_id app_id union_id = do
    modify $ over wxppAcidStateCachedUnionID $
                Map.insert (open_id, app_id) (union_id, now)

-- | 查找 UnionID 缓存
wxppAcidLookupCachedUnionID ::
    WxppOpenID -> WxppAppID -> Query WxppAcidState (Maybe (WxppUnionID, UTCTime))
wxppAcidLookupCachedUnionID open_id app_id =
    asks $ (Map.lookup (open_id, app_id)) . _wxppAcidStateCachedUnionID

$(makeAcidic ''WxppAcidState
    [ 'wxppAcidGetAcccessTokens
    , 'wxppAcidGetAcccessToken
    , 'wxppAcidAddAcccessToken
    , 'wxppAcidPurgeAcccessToken
    , 'wxppAcidLookupMediaIDByHash
    , 'wxppAcidSetCachedUnionID
    , 'wxppAcidLookupCachedUnionID
    ])


wxppAcidGetUsableAccessToken :: MonadIO m =>
    AcidState WxppAcidState -> m (Maybe AccessToken)
wxppAcidGetUsableAccessToken acid = liftIO $ do
    now <- getCurrentTime
    fmap (join . (fmap $ \x -> if snd x > now then Just (fst x) else Nothing)) $
        query acid WxppAcidGetAcccessToken
