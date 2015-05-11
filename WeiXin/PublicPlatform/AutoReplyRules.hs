module WeiXin.PublicPlatform.AutoReplyRules
    ( module WeiXin.PublicPlatform.AutoReplyRules
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Control.Monad.Logger
import Data.Aeson

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS


-- | 获取自动回复规则
wxppQueryOriginAutoReplyRules :: ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken
    -> m Object
wxppQueryOriginAutoReplyRules (AccessToken { accessTokenData = atk }) = do
    let url = wxppRemoteApiBaseUrl <> "/get_current_autoreply_info"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ getWith opts url)
            >>= asWxppWsResponseNormal'
