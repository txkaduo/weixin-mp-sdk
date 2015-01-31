module WeiXin.PublicPlatform.EndUser where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Control.Monad.Logger

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Error
import WeiXin.PublicPlatform.WS


-- | 调用服务器接口，查询用户基础信息
wxppQueryEndUserInfo ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> WxppOpenID -> m EndUserQueryResult
wxppQueryEndUserInfo (AccessToken access_token) (WxppOpenID open_id) = do
    let url = wxppRemoteApiBaseUrl <> "/user/info"
        opts = defaults & param "access_token" .~ [ access_token ]
                        & param "openid" .~ [ open_id ]
                        & param "lang" .~ [ "zh_CN" :: Text ]
    (liftIO $ getWith opts url)
                >>= asWxppWsResponseNormal'


