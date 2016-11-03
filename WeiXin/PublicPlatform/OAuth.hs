{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.OAuth
    ( OAuthCode(..)
    , deniedOAuthCode
    , oauthScopeCanGetUserInfo
    , oauthScopeAccessTokenWithUnionID
    , oauthScopeMinForUnionID
    , oauthScopeIsUnknown
    , WxppAuthConfig(..)
    , OAuthAccessToken(..)
    , OAuthAccessTokenPkg(..)
    , OAuthRefreshToken(..)
    , OAuthAccessTokenResult(..)
    , OAuthRefreshAccessTokenResult(..)
    , OAuthGetUserInfoResult(..)
    , wxppOAuthRequestAuthInsideWx
    , wxppOAuthRequestAuthOutsideWx
    , wxppOAuthGetAccessToken
    , wxppOAuthRefreshAccessToken
    , wxppOAuthGetUserInfo
    , wxppOAuthGetUserInfo'
    , wxppOAuthCheckAccessToken
    , wxppOAuthGetUserInfoCached
    ) where

import ClassyPrelude
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens
import Control.Monad.Reader                 (asks)
import Network.URI                          ( parseAbsoluteURI, uriQuery, uriFragment
                                            , uriToString
                                            )
import Network.HTTP                         (urlEncodeVars)
import Text.Shakespeare.I18N                (Lang)
import Data.Time                            (NominalDiffTime)
import Data.Aeson                           (FromJSON(..), withObject, (.:))

import Yesod.Helpers.Parsec


import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS


-- | 文档说用户拒绝授权, code 为空,实际上会出现 authdeny 这样的字串
deniedOAuthCode :: OAuthCode -> Bool
deniedOAuthCode (OAuthCode x) = null x || x == "authdeny"


-- | 某个 scope 是否有权限获取用户基本信息
oauthScopeCanGetUserInfo :: OAuthScope -> Bool
oauthScopeCanGetUserInfo AS_SnsApiUserInfo = True
oauthScopeCanGetUserInfo AS_SnsApiLogin    = True
oauthScopeCanGetUserInfo _                 = False


-- | 有些 scope 使用时，在 OAuthAccessTokenResult 自带unionid 字段
oauthScopeAccessTokenWithUnionID :: OAuthScope -> Bool
oauthScopeAccessTokenWithUnionID AS_SnsApiUserInfo = True
oauthScopeAccessTokenWithUnionID AS_SnsApiLogin    = True
oauthScopeAccessTokenWithUnionID _                 = False


-- | 能取得 union id 的最低 OAuthScope 要求．
-- 仅对于微信内使用有意义，因为微信外只有一个 AS_SnsApiLogin 可用
-- 这个函数存在是因为文档对于这个问题前后有变化，担心以后也可能变化
-- 不想其它代码写死这部分逻辑
oauthScopeMinForUnionID :: OAuthScope
oauthScopeMinForUnionID = AS_SnsApiUserInfo


-- | 如果是未知的 scope 则得到Just
oauthScopeIsUnknown :: OAuthScope -> Maybe Text
oauthScopeIsUnknown (AS_Unknown t) = Just t
oauthScopeIsUnknown _              = Nothing



-- | 当仅需要做 OAuth 认证时，不需要 WxppAppConfig 那么多信息
-- 这个相当于为 OAuth 简化的 WxppAppConfig
data WxppAuthConfig = WxppAuthConfig {
                        wxppAuthAppID           :: WxppAppID
                        , wxppAuthAppSecret     :: WxppAppSecret
                        }
                        deriving (Show, Eq)

instance FromJSON WxppAuthConfig where
    -- 注意：这个实现刻意地跟 WxppAppConfig 的实现兼容
    parseJSON = withObject "WxppAuthConfig" $ \obj -> do
                    WxppAuthConfig
                        <$> obj .: "app-id"
                        <*> obj .: "secret"


-- | 获取用户授权: 仅用于微信内打开
wxppOAuthRequestAuthInsideWx :: Maybe WxppAppID
                             -- ^ 如果我方是第三方平台中服务方，需指定此参数
                             -> WxppAppID
                             -> OAuthScope
                             -> UrlText      -- ^ return to this url
                             -> Text -- ^ state to return
                             -> UrlText
wxppOAuthRequestAuthInsideWx =
    wxppOAuthRequestAuthImpl
      "https://open.weixin.qq.com/connect/oauth2/authorize"


-- | 获取用户授权: 用于微信外游览器
wxppOAuthRequestAuthOutsideWx :: WxppAppID
                              -> UrlText      -- ^ return to this url
                              -> Text -- ^ state to return
                              -> UrlText
wxppOAuthRequestAuthOutsideWx app_id =
    wxppOAuthRequestAuthImpl
      "https://open.weixin.qq.com/connect/qrconnect"
      Nothing
      app_id
      AS_SnsApiLogin


-- | 微信开发开台有几个地方使用 oauth2 授权，它们非常相近，又略有不同
-- 这个函数尽量兼容所有需求
wxppOAuthRequestAuthImpl :: String
                         -> Maybe WxppAppID
                         -- ^ 如果我方是第三方平台中服务方，需指定此参数
                         -> WxppAppID
                         -> OAuthScope
                         -> UrlText      -- ^ return to this url
                         -> Text -- ^ state to return
                         -> UrlText
wxppOAuthRequestAuthImpl api_url m_comp_app_id app_id scope return_url state =
    UrlText $ fromString $ uriToString id uri ""
    where
        base_uri = fromMaybe (error "cannot parse uri") $
                    parseAbsoluteURI api_url

        vars = catMaybes
                [ Just ("appid",         unpack (unWxppAppID app_id))
                , Just ("redirect_uri",  unpack (unUrlText return_url))
                , Just ("response_type", "code")
                , Just ("scope",         simpleEncode scope)
                , Just ("state",         unpack state)
                , fmap (("component_appid",) . unpack . unWxppAppID) m_comp_app_id
                ]

        uri = base_uri
                { uriQuery      = add_qmark (urlEncodeVars vars)
                , uriFragment   = "#wechat_redirect"
                }
        add_qmark s = case s of
                        []    -> s
                        '?':_ -> s
                        _     -> '?' : s


-- | 根据 code 取 access token
-- CAUTION: 不可用于第三方平台, see wxppTpOAuthGetAccessToken
wxppOAuthGetAccessToken :: (WxppApiMonad env m)
                        => WxppAppID
                        -> WxppAppSecret
                        -> OAuthCode
                        -> m OAuthAccessTokenResult
wxppOAuthGetAccessToken app_id secret code = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSnsApiBase url_conf <> "/oauth2/access_token"
        opts = defaults & param "appid" .~ [ unWxppAppID app_id ]
                        & param "secret" .~ [ unWxppAppSecret secret ]
                        & param "code" .~ [ unOAuthCode code ]
                        & param "grant_type" .~ [ "authorization_code" ]

    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseNormal'


-- | Refresh AccessToken
-- CAUTION: 不可用于第三方平台, see wxppTpOAuthRefreshAccessToken
wxppOAuthRefreshAccessToken :: (WxppApiMonad env m)
                            => WxppAppID
                            -> OAuthRefreshToken
                            -> m OAuthRefreshAccessTokenResult
wxppOAuthRefreshAccessToken app_id rtk = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSnsApiBase url_conf <> "/oauth2/refresh_token"
        opts = defaults & param "appid" .~ [ unWxppAppID app_id ]
                        & param "refresh_token" .~ [ unOAuthRefreshToken rtk ]
                        & param "grant_type" .~ [ "refresh_token" ]

    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseNormal'


wxppOAuthGetUserInfo :: (WxppApiMonad env m)
                    => Lang
                    -> OAuthAccessTokenPkg
                    -> m OAuthGetUserInfoResult
wxppOAuthGetUserInfo lang atk_p = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSnsApiBase url_conf <> "/userinfo"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken $ oauthAtkPRaw atk_p ]
                        & param "openid" .~ [ unWxppOpenID $ oauthAtkPOpenID atk_p ]
                        & param "lang" .~ [ lang ]
    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseNormal'


wxppOAuthGetUserInfo' :: (WxppApiMonad env m)
                      => OAuthAccessTokenPkg
                      -> m OAuthGetUserInfoResult
wxppOAuthGetUserInfo' = wxppOAuthGetUserInfo "zh_CN"


-- | call wxppOAuthGetUserInfo and save it in cache
wxppOAuthGetUserInfoCached :: (WxppApiMonad env m, WxppCacheTemp c)
                            => c
                            -> NominalDiffTime
                            -> Lang
                            -> OAuthAccessTokenPkg
                            -> m OAuthGetUserInfoResult
wxppOAuthGetUserInfoCached cache ttl lang atk_p = do
    m_info <- wxppGetSnsUserInfoCached cache ttl app_id open_id lang
    case m_info of
        Just info -> return info
        Nothing -> do
            info <- wxppOAuthGetUserInfo lang atk_p
            liftIO $ wxppCacheAddSnsUserInfo cache app_id lang info
            return info
    where
        app_id = oauthAtkPAppID atk_p
        open_id = oauthAtkPOpenID atk_p


wxppOAuthCheckAccessToken :: (WxppApiMonad env m)
                            => OAuthAccessTokenPkg
                            -> m ()
wxppOAuthCheckAccessToken atk_p = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSnsApiBase url_conf <> "/auth"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken $ oauthAtkPRaw atk_p ]
                        & param "openid" .~ [ unWxppOpenID $ oauthAtkPOpenID atk_p ]

    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseVoid
