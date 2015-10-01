{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.OAuth
    ( OAuthCode(..)
    , OAuthAccessToken(..)
    , OAuthRefreshToken(..)
    , OAuthAccessTokenResult(..)
    , OAuthRefreshAccessTokenResult(..)
    , OAuthGetUserInfoResult(..)
    , wxppOAuthRequestAuth
    , wxppOAuthGetAccessToken
    , wxppOAuthRefreshAccessToken
    , wxppOAuthGetUserInfo
    , wxppOAuthCheckAccessToken
    ) where

import ClassyPrelude
import qualified Data.Text                  as T
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Time                            (NominalDiffTime)
import Yesod.Core                           (PathPiece(..))
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..))
import Network.URI                          ( parseAbsoluteURI, uriQuery, uriFragment
                                            , uriToString
                                            )
import Network.HTTP                         (urlEncodeVars)
import Text.Shakespeare.I18N                (Lang)

import Yesod.Helpers.Parsec
import Yesod.Helpers.Utils                  (emptyTextToNothing)


import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS



data OAuthScope = AS_SnsApiBase
                | AS_SnsApiUserInfo
                deriving (Eq, Ord, Enum, Bounded)

$(derivePersistFieldS "OAuthScope")

instance SimpleStringRep OAuthScope where
    -- Encode values will be used in wxppAuthPageUrl
    -- so they must be consistent with WX doc.
    simpleEncode AS_SnsApiBase      = "snsapi_base"
    simpleEncode AS_SnsApiUserInfo  = "snsapi_userinfo"

    simpleParser = makeSimpleParserByTable
                    [ ("snsapi_base", AS_SnsApiBase)
                    , ("snsapi_userinfo", AS_SnsApiUserInfo)
                    ]


newtype OAuthCode = OAuthCode { unOAuthCode :: Text }
                    deriving (Eq, Ord, Show, PersistField, PersistFieldSql)

instance PathPiece OAuthCode where
    fromPathPiece = fmap OAuthCode . fromPathPiece
    toPathPiece = toPathPiece . unOAuthCode

instance ToJSON OAuthCode where toJSON = toJSON . unOAuthCode


newtype OAuthAccessToken = OAuthAccessToken { unOAuthAccessToken :: Text }
                        deriving (Eq, Ord, Show, PersistField, PersistFieldSql)

instance PathPiece OAuthAccessToken where
    fromPathPiece = fmap OAuthAccessToken . fromPathPiece
    toPathPiece = toPathPiece . unOAuthAccessToken

instance FromJSON OAuthAccessToken where
    parseJSON = fmap OAuthAccessToken . parseJSON


newtype OAuthRefreshToken = OAuthRefreshToken { unOAuthRefreshToken :: Text }
                            deriving (Eq, Ord, Show, PersistField, PersistFieldSql)


instance PathPiece OAuthRefreshToken where
    fromPathPiece = fmap OAuthRefreshToken . fromPathPiece
    toPathPiece = toPathPiece . unOAuthRefreshToken

instance FromJSON OAuthRefreshToken where parseJSON = fmap OAuthRefreshToken . parseJSON

instance ToJSON OAuthRefreshToken where toJSON = toJSON . unOAuthRefreshToken


data OAuthAccessTokenResult = OAuthAccessTokenResult {
                                oauthAtkToken           :: OAuthAccessToken
                                , oauthAtkTTL           :: NominalDiffTime
                                , oauthAtkScope         :: [Text]
                                , oauthAtkRefreshToken  :: OAuthRefreshToken
                                , oauthAtkOpenID        :: WxppOpenID
                                , oauthAtkUnionID       :: Maybe WxppUnionID
                                }
                                deriving (Eq, Show)

instance FromJSON OAuthAccessTokenResult where
    parseJSON = withObject "OAuthAccessTokenResult" $ \o -> do
                    OAuthAccessTokenResult
                        <$> o .: "access_token"
                        <*> ((fromIntegral :: Int -> NominalDiffTime) <$> o .: "expires_in")
                        <*> ((map T.strip . T.splitOn ",") <$> o .: "scope")
                        <*> o .: "refresh_token"
                        <*> o .: "openid"
                        <*> (fmap WxppUnionID . join . fmap emptyTextToNothing <$> o .:? "unionid")


data OAuthRefreshAccessTokenResult = OAuthRefreshAccessTokenResult {
                                        oauthRtkToken           :: OAuthAccessToken
                                        , oauthRtkTTL           :: NominalDiffTime
                                        , oauthRtkScope         :: [Text]
                                        , ouahtRtkRefreshToken  :: OAuthRefreshToken
                                        , oauthRtkOpenID        :: WxppOpenID
                                        }
                                        deriving (Eq, Show)

instance FromJSON OAuthRefreshAccessTokenResult where
    parseJSON = withObject "OAuthRefreshAccessTokenResult" $ \o -> do
                    OAuthRefreshAccessTokenResult
                        <$> o .: "access_token"
                        <*> ((fromIntegral :: Int -> NominalDiffTime) <$> o .: "expires_in")
                        <*> ((map T.strip . T.splitOn ",") <$> o .: "scope")
                        <*> o .: "refresh_token"
                        <*> o .: "openid"

data OAuthGetUserInfoResult = OAuthGetUserInfoResult {
                                oauthUserInfoOpenID         :: WxppOpenID
                                , oauthUserInfoNickname     :: Text
                                , oauthUserInfoGender       :: Maybe Gender
                                , oauthUserInfoProvice      :: Text
                                , oauthUserInfoCity         :: Text
                                , oauthUserInfoCountry      :: Text
                                , oauthUserInfoHeadImgUrl   :: Maybe UrlText
                                , oauthUserInfoPrivileges   :: [Text]
                                , oauthUserInfoUnionID      :: Maybe WxppUnionID
                                }
                                deriving (Eq, Show)

instance FromJSON OAuthGetUserInfoResult where
    parseJSON = withObject "OAuthGetUserInfoResult" $ \o -> do
                    OAuthGetUserInfoResult
                        <$> o .: "openid"
                        <*> o .: "nickname"
                        <*> (o .: "sex" >>= parseSexJson)
                        <*> o .: "province"
                        <*> o .: "city"
                        <*> o .: "country"
                        <*> (fmap UrlText . join . fmap emptyTextToNothing <$> o .:? "headimgurl")
                        <*> o .: "privilege"
                        <*> (fmap WxppUnionID . join . fmap emptyTextToNothing <$> o .:? "unionid")

-- | 获取用户授权
wxppOAuthRequestAuth :: WxppAppID
                    -> OAuthScope
                    -> UrlText      -- ^ return to this url
                    -> Maybe String -- ^ state to return
                    -> UrlText
wxppOAuthRequestAuth app_id scope return_url m_state =
    UrlText $ fromString $ uriToString id uri ""
    where
        base_uri = fromMaybe (error "cannot parse uri") $
                    parseAbsoluteURI "https://open.weixin.qq.com/connect/oauth2/authorize"

        vars = catMaybes
                [ Just ("appid",         T.unpack (unWxppAppID app_id))
                , Just ("redirect_uri",  T.unpack (unUrlText return_url))
                , Just ("response_type", "code")
                , Just ("scope",         simpleEncode scope)
                , flip fmap m_state $ \state -> ("state", state)
                ]

        uri = base_uri
                { uriQuery      = urlEncodeVars vars
                , uriFragment   = "#wechat_redirect"
                }


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
                    -> OAuthAccessToken
                    -> WxppOpenID
                    -> m OAuthGetUserInfoResult
wxppOAuthGetUserInfo lang atk open_id = do
    let url = "https://api.weixin.qq.com/sns/userinfo"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken atk ]
                        & param "openid" .~ [ unWxppOpenID open_id ]
                        & param "lang" .~ [ lang ]
    liftIO (getWith opts url)
        >>= asWxppWsResponseNormal'


wxppOAuthCheckAccessToken :: (MonadIO m, MonadThrow m)
                            => OAuthAccessToken
                            -> WxppOpenID
                            -> m ()
wxppOAuthCheckAccessToken atk open_id = do
    let url = "https://api.weixin.qq.com/sns/auth"
        opts = defaults & param "access_token" .~ [ unOAuthAccessToken atk ]
                        & param "openid" .~ [ unWxppOpenID open_id ]
    liftIO (getWith opts url)
        >>= asWxppWsResponseVoid
