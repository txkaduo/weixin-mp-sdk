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

import WeiXin.PublicPlatform.Types


-- | 服务器进程需持久保持的数据
data WxppAcidState = WxppAcidState {
                    _wxppAcidStateAccessTokens :: ![(AccessToken, UTCTime)]
                        -- ^ access token 更新允许有个过滤期
                        -- 因此记录了最近使用过的所有 AccessToken 及其过期时间
                        -- 这个列表实现时没有明确地限制长度
                        -- 而是定时丢弃过期的 access token
                    , _wxppAcidStateUploadedMedia :: !(Map MD5Hash UploadResult)
                    }
                    deriving (Typeable)

$(makeLenses ''WxppAcidState)
$(deriveSafeCopy 0 'base ''WxppAcidState)

instance Default WxppAcidState where
    def = WxppAcidState def def

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

$(makeAcidic ''WxppAcidState
    [ 'wxppAcidGetAcccessTokens
    , 'wxppAcidGetAcccessToken
    , 'wxppAcidAddAcccessToken
    , 'wxppAcidPurgeAcccessToken
    , 'wxppAcidLookupMediaIDByHash
    ])
