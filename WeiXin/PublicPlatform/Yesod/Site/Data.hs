module WeiXin.PublicPlatform.Yesod.Site.Data where

import Yesod
import Prelude

import WeiXin.PublicPlatform.Security

data WxppSub = WxppSub {
                wxppSubAppConfig    :: WxppAppConfig
                    -- ^ 所有配置信息
                , wxppSubMsgHandler :: WxppInMsgHandler IO
                }

instance Show WxppSub where
    show (WxppSub app_config _) =
        "WxppSub: " ++ show app_config

mkYesodSubData "WxppSub" [parseRoutes|
/ SubHomeR GET POST
|]
