module WeiXin.PublicPlatform.AutoReplyRules
    ( module WeiXin.PublicPlatform.AutoReplyRules
    , module WeiXin.PublicPlatform.Types
    ) where

import ClassyPrelude
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens
import Data.Aeson

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS


-- | 获取自动回复规则
wxppQueryOriginAutoReplyRules :: (WxppApiMonad m)
                              => AccessToken
                              -> m Object
wxppQueryOriginAutoReplyRules (AccessToken { accessTokenData = atk }) = do
    let url = wxppRemoteApiBaseUrl <> "/get_current_autoreply_info"
        opts = defaults & param "access_token" .~ [ atk ]

    sess <- ask
    liftIO (WS.getWith opts sess url)
            >>= asWxppWsResponseNormal'
