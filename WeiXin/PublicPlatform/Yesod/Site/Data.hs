module WeiXin.PublicPlatform.Yesod.Site.Data where

import Yesod
import Prelude

import WeiXin.PublicPlatform.Security

data WxppSub = WxppSub {
                wxppSubAppConfig        :: WxppAppConfig
                    -- ^ 所有配置信息
                , wxppSubAccessTokens   :: IO (Maybe AccessToken)
                    -- ^ a computation to get usable access token
                , wxppSubMsgHandler     :: WxppInMsgHandler IO
                }

instance Show WxppSub where
    show (WxppSub app_config _ _) =
        "WxppSub: " ++ show app_config

mkYesodSubData "WxppSub" [parseRoutes|
/msg            MessageR        GET POST
/menu/reload    ReloadMenuR     GET
/menu/query     QueryMenuR      GET
|]
