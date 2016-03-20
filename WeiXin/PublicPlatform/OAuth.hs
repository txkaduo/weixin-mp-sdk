{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.OAuth
    ( OAuthCode(..)
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
    , wxppOAuthCheckAccessToken
    , wxppOAuthGetUserInfoCached
    ) where

import ClassyPrelude
import qualified Data.Text                  as T
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens
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
wxppOAuthRequestAuthInsideWx :: WxppAppID
                            -> OAuthScope
                            -> UrlText      -- ^ return to this url
                            -> Text -- ^ state to return
                            -> UrlText
wxppOAuthRequestAuthInsideWx =
    wxppOAuthRequestAuthImpl "https://open.weixin.qq.com/connect/oauth2/authorize"


-- | 获取用户授权: 用于微信外游览器
wxppOAuthRequestAuthOutsideWx :: WxppAppID
                                -> UrlText      -- ^ return to this url
                                -> Text -- ^ state to return
                                -> UrlText
wxppOAuthRequestAuthOutsideWx app_id =
    wxppOAuthRequestAuthImpl "https://open.weixin.qq.com/connect/qrconnect" app_id AS_SnsApiLogin

wxppOAuthRequestAuthImpl :: String
                        -> WxppAppID
                        -> OAuthScope
                        -> UrlText      -- ^ return to this url
                        -> Text -- ^ state to return
                        -> UrlText
wxppOAuthRequestAuthImpl api_url app_id scope return_url state =
    UrlText $ fromString $ uriToString id uri ""
    where
        base_uri = fromMaybe (error "cannot parse uri") $
                    parseAbsoluteURI api_url

        vars =  [ ("appid",         T.unpack (unWxppAppID app_id))
                , ("redirect_uri",  T.unpack (unUrlText return_url))
                , ("response_type", "code")
                , ("scope",         simpleEncode scope)
                , ("state",         T.unpack state)
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
wxppOAuthGetAccessToken :: (WxppApiMonad m)
                        => WxppAppID
                        -> WxppAppSecret
                        -> OAuthCode
                        -> m OAuthAccessTokenResult
wxppOAuthGetAccessToken app_id secret code = do
    let url = "https://api.weixin.qq.com/sns/oauth2/access_token"
        opts = defaults & param "appid" .~ [ unWxppAppID app_id ]
                        & param "secret" .~ [ unWxppAppSecret secret ]
                        & param "code" .~ [ unOAuthCode code ]
                        & param "grant_type" .~ [ "authorization_code" ]

    sess <- ask
    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseNormal'


-- | Refresh AccessToken
wxppOAuthRefreshAccessToken :: (WxppApiMonad m)
                            => WxppAppID
                            -> OAuthRefreshToken
                            -> m OAuthRefreshAccessTokenResult
wxppOAuthRefreshAccessToken app_id rtk = do
    let url = "https://api.weixin.qq.com/sns/oauth2/refresh_token"
        opts = defaults & param "appid" .~ [ unWxppAppID app_id ]
                        & param "refresh_token" .~ [ unOAuthRefreshToken rtk ]
                        & param "grant_type" .~ [ "refresh_token" ]

    sess <- ask
    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseNormal'


wxppOAuthGetUserInfo :: (WxppApiMonad m)
                    => Lang
                    -> OAuthAccessTokenPkg
                    -> m OAuthGetUserInfoResult
wxppOAuthGetUserInfo lang atk_p = do
    let url = "https://api.weixin.qq.com/sns/userinfo"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken $ oauthAtkPRaw atk_p ]
                        & param "openid" .~ [ unWxppOpenID $ oauthAtkPOpenID atk_p ]
                        & param "lang" .~ [ lang ]
    sess <- ask
    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseNormal'

-- | call wxppOAuthGetUserInfo and save it in cache
wxppOAuthGetUserInfoCached :: (WxppApiMonad m, WxppCacheTemp c)
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
            now <- liftIO getCurrentTime
            liftIO $ wxppCacheAddSnsUserInfo cache app_id lang info now
            return info
    where
        app_id = oauthAtkPAppID atk_p
        open_id = oauthAtkPOpenID atk_p

wxppOAuthCheckAccessToken :: (WxppApiMonad m)
                            => OAuthAccessTokenPkg
                            -> m ()
wxppOAuthCheckAccessToken atk_p = do
    let url = "https://api.weixin.qq.com/sns/auth"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken $ oauthAtkPRaw atk_p ]
                        & param "openid" .~ [ unWxppOpenID $ oauthAtkPOpenID atk_p ]

    sess <- ask
    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseVoid
