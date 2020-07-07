module WeiXin.PublicPlatform.AutoReplyRules
    ( module WeiXin.PublicPlatform.AutoReplyRules
    , module WeiXin.PublicPlatform.Class
    ) where

import ClassyPrelude
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens
#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Reader                 (asks)
#endif
import Data.Aeson

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS


-- | 获取自动回复规则
wxppQueryOriginAutoReplyRules :: (WxppApiMonad env m)
                              => AccessToken
                              -> m Object
wxppQueryOriginAutoReplyRules (AccessToken { accessTokenData = atk }) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/get_current_autoreply_info"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.getWith opts sess url)
            >>= asWxppWsResponseNormal'
