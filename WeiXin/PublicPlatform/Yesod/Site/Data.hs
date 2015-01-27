module WeiXin.PublicPlatform.Yesod.Site.Data where

import Yesod
import Prelude

import WeiXin.PublicPlatform.Security

data WxppSub = WxppSub {
                wxppSubAppConfig    :: WxppAppConfig
                    -- ^ 所有配置信息
                , wxppSubMsgHandler :: WxppAppConfig
                                        -> WxppInMsgEntity
                                        -> IO (Either String (Maybe WxppOutMsg))
                    -- ^ 响应收到的服务器信息
                    -- Left 用于表达错误
                    -- Right Nothing 代表无需回复一个新信息
                }

instance Show WxppSub where
    show (WxppSub app_config _) =
        "WxppSub: " ++ show app_config

mkYesodSubData "WxppSub" [parseRoutes|
/ SubHomeR GET POST
|]
