{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.OAuth
    ( OAuthCode(..)
    , OAuthAccessToken(..)
    , OAuthAccessTokenPkg(..)
    , OAuthRefreshToken(..)
    , OAuthAccessTokenResult(..)
    , OAuthRefreshAccessTokenResult(..)
    , OAuthGetUserInfoResult(..)
    , wxppOAuthRequestAuth
    , wxppOAuthGetAccessToken
    , wxppOAuthRefreshAccessToken
    , wxppOAuthGetUserInfo
    , wxppOAuthCheckAccessToken
    , wxppOAuthGetUserInfoCached
    ) where

import ClassyPrelude
import qualified Data.Text                  as T
import Network.Wreq
import Control.Lens
import Network.URI                          ( parseAbsoluteURI, uriQuery, uriFragment
                                            , uriToString
                                            )
import Network.HTTP                         (urlEncodeVars)
import Text.Shakespeare.I18N                (Lang)
import Data.Time                            (NominalDiffTime)

import Yesod.Helpers.Parsec


import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS



-- | 获取用户授权
wxppOAuthRequestAuth :: WxppAppID
                    -> OAuthScope
                    -> UrlText      -- ^ return to this url
                    -> Text -- ^ state to return
                    -> UrlText
wxppOAuthRequestAuth app_id scope return_url state =
    UrlText $ fromString $ uriToString id uri ""
    where
        base_uri = fromMaybe (error "cannot parse uri") $
                    parseAbsoluteURI "https://open.weixin.qq.com/connect/oauth2/authorize"

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
wxppOAuthGetAccessToken :: (MonadIO m, MonadThrow m)
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
    liftIO (getWith opts url)
        >>= asWxppWsResponseNormal'


-- | Refresh AccessToken
wxppOAuthRefreshAccessToken :: (MonadIO m, MonadThrow m)
                            => WxppAppID
                            -> OAuthRefreshToken
                            -> m OAuthRefreshAccessTokenResult
wxppOAuthRefreshAccessToken app_id rtk = do
    let url = "https://api.weixin.qq.com/sns/oauth2/refresh_token"
        opts = defaults & param "appid" .~ [ unWxppAppID app_id ]
                        & param "refresh_token" .~ [ unOAuthRefreshToken rtk ]
                        & param "grant_type" .~ [ "refresh_token" ]
    liftIO (getWith opts url)
        >>= asWxppWsResponseNormal'


wxppOAuthGetUserInfo :: (MonadIO m, MonadThrow m)
                    => Lang
                    -> OAuthAccessTokenPkg
                    -> m OAuthGetUserInfoResult
wxppOAuthGetUserInfo lang atk_p = do
    let url = "https://api.weixin.qq.com/sns/userinfo"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken $ oauthAtkPRaw atk_p ]
                        & param "openid" .~ [ unWxppOpenID $ oauthAtkPOpenID atk_p ]
                        & param "lang" .~ [ lang ]
    liftIO (getWith opts url)
        >>= asWxppWsResponseNormal'

-- | call wxppOAuthGetUserInfo and save it in cache
wxppOAuthGetUserInfoCached :: (MonadIO m, MonadThrow m, WxppCacheTemp c)
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

wxppOAuthCheckAccessToken :: (MonadIO m, MonadThrow m)
                            => OAuthAccessTokenPkg
                            -> m ()
wxppOAuthCheckAccessToken atk_p = do
    let url = "https://api.weixin.qq.com/sns/auth"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken $ oauthAtkPRaw atk_p ]
                        & param "openid" .~ [ unWxppOpenID $ oauthAtkPOpenID atk_p ]
    liftIO (getWith opts url)
        >>= asWxppWsResponseVoid
