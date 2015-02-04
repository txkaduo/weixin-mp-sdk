module WeiXin.PublicPlatform.EndUser where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Control.Monad.Logger
import Data.Aeson
import Data.Conduit                         (Source, yield)

import WeiXin.PublicPlatform.Types
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


data GetUserResult = GetUserResult
                        Int             -- ^ total
                        Int             -- ^ count
                        [WxppOpenID]
                        (Maybe WxppOpenID)
                                -- ^ next open id


instance FromJSON GetUserResult where
    parseJSON = withObject "GetUserResult" $ \obj -> do
                    total <- obj .: "total"
                    count <- obj .: "count"
                    lst <- obj .: "data" >>=
                            ( withObject "data" $ \o -> do
                                map WxppOpenID <$> o .: "openid"
                            )
                    next_openid <- fmap WxppOpenID <$> obj .:? "next_openid"
                    return $ GetUserResult
                                total count lst next_openid


-- | 调用服务器接口，查询所有订阅用户
wxppGetEndUserSource ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> Source m GetUserResult
wxppGetEndUserSource (AccessToken access_token) = loop Nothing
    where
        url         = wxppRemoteApiBaseUrl <> "/user/get"
        loop m_start_id = do
            let opts = defaults & param "access_token" .~ [ access_token ]
                                & (case m_start_id of
                                    Nothing     -> id
                                    Just (WxppOpenID start_open_id) ->
                                        param "next_openid" .~ [ start_open_id ]
                                    )
            r@(GetUserResult _ _ _ m_next_id) <-
                (liftIO $ getWith opts url) >>= asWxppWsResponseNormal'
            yield r
            maybe (return ()) (loop . Just) $ m_next_id
