module WeiXin.PublicPlatform.EndUser where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Control.Monad.Logger
import Data.Aeson
import Data.Conduit                         (Source, yield)
import Data.Acid
import Data.Time                            (diffUTCTime, NominalDiffTime)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Acid


-- | 调用服务器接口，查询用户基础信息
wxppQueryEndUserInfo ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> WxppOpenID -> m EndUserQueryResult
wxppQueryEndUserInfo (AccessToken { accessTokenData = atk }) (WxppOpenID open_id) = do
    let url = wxppRemoteApiBaseUrl <> "/user/info"
        opts = defaults & param "access_token" .~ [ atk ]
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
wxppGetEndUserSource (AccessToken { accessTokenData = atk }) = loop Nothing
    where
        url         = wxppRemoteApiBaseUrl <> "/user/get"
        loop m_start_id = do
            let opts = defaults & param "access_token" .~ [ atk ]
                                & (case m_start_id of
                                    Nothing     -> id
                                    Just (WxppOpenID start_open_id) ->
                                        param "next_openid" .~ [ start_open_id ]
                                    )
            r@(GetUserResult _ _ _ m_next_id) <-
                (liftIO $ getWith opts url) >>= asWxppWsResponseNormal'
            yield r
            maybe (return ()) (loop . Just) $ m_next_id


-- | 取用户的 UnionID
-- 先从缓存找，找不到或找到的记录太旧，则调用接口
-- 如果调用接口取得最新的数据，立刻缓存之
wxppCachedGetEndUserUnionID ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    NominalDiffTime
    -> AcidState WxppAcidState
    -> AccessToken
    -> WxppOpenID
    -> m (Maybe WxppUnionID)
wxppCachedGetEndUserUnionID ttl acid atk open_id = do
    m_res <- liftIO $ query acid $ WxppAcidLookupCachedUnionID open_id app_id
    now <- liftIO getCurrentTime
    m_uid0 <- case m_res of
                Just (union_id, ctime) -> do
                    if diffUTCTime now ctime > ttl
                        then return Nothing
                        else return $ Just union_id

                Nothing -> return Nothing

    case m_uid0 of
        Just uid    -> return $ Just uid

        Nothing     -> do
            qres <- wxppQueryEndUserInfo atk open_id
            case qres of
                EndUserQueryResultNotSubscribed {} -> do
                    $logWarnS wxppLogSource $
                        "wxppCachedGetEndUserUnionID failed because user does not subscribe: "
                            <> unWxppOpenID open_id
                    return Nothing

                EndUserQueryResult _ _ _ _ _ _ _ _ _ m_uid -> do
                    forM_ m_uid $ \uid -> liftIO $ do
                            update acid $ WxppAcidSetCachedUnionID now open_id app_id uid

                    return m_uid
    where
        app_id = accessTokenApp atk
