module WeiXin.PublicPlatform.Yesod.Site.Data where

import Yesod

import WeiXin.PublicPlatform.Security

data WxppSub = WxppSub {
                wxppSubToken        :: Token
                , wxppSubAesKey     :: AesKey
                }

mkYesodSubData "WxppSub" [parseRoutes|
/ SubHomeR GET
|]
