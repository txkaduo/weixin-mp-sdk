module WeiXin.PublicPlatform.EndUser where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Control.Monad.Logger
import Data.Aeson
import Data.Conduit                         (Source, yield)
import Data.Time                            (diffUTCTime, NominalDiffTime)
import qualified Data.Text                  as T

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS

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

                    -- 当没数据时，似乎平台会不发出 data 字段
                    m_data_obj <- obj .:? "data"
                    lst <- case m_data_obj of
                            Nothing -> return []
                            Just o  -> map WxppOpenID <$> o .: "openid"

                    next_openid <- fmap WxppOpenID <$> obj .:? "next_openid"
                    return $ GetUserResult
                                total count lst next_openid

wxppOpenIdListInGetUserResult :: GetUserResult -> [WxppOpenID]
wxppOpenIdListInGetUserResult (GetUserResult _ _ x _) = x

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

            -- 平台是用空字串表示结束的
            let m_next_id' = do
                    oid <- m_next_id
                    if T.null $ T.strip $ unWxppOpenID oid
                        then mzero
                        else m_next_id

            maybe (return ()) (loop . Just) $ m_next_id'


-- | 取用户的 UnionID
-- 先从缓存找，找不到或找到的记录太旧，则调用接口
-- 如果调用接口取得最新的数据，立刻缓存之
wxppCachedGetEndUserUnionID ::
    ( MonadIO m, MonadLogger m, MonadThrow m, WxppCacheBackend c) =>
    c
    -> NominalDiffTime
    -> AccessToken
    -> WxppOpenID
    -> m (Maybe WxppUnionID)
wxppCachedGetEndUserUnionID cache ttl atk open_id = do
    liftM endUserQueryResultUnionID $ wxppCachedQueryEndUserInfo cache ttl atk open_id

wxppCachedQueryEndUserInfo ::
    (MonadIO m, MonadLogger m, MonadThrow m, WxppCacheBackend c) =>
    c
    -> NominalDiffTime
    -> AccessToken
    -> WxppOpenID
    -> m EndUserQueryResult
wxppCachedQueryEndUserInfo cache ttl atk open_id = do
    m_res <- liftIO $ wxppCacheLookupUserInfo cache app_id open_id
    now <- liftIO getCurrentTime
    let m_qres0 = case m_res of
                    Just ((EndUserQueryResultNotSubscribed {}), _) ->
                        -- never do negative cache
                        Nothing

                    Just (qres, ctime) ->
                        if diffUTCTime now ctime > ttl
                            then Nothing
                            else Just qres

                    Nothing -> Nothing


    case m_qres0 of
        Just qres    -> return qres

        Nothing     -> do
            qres <- wxppQueryEndUserInfo atk open_id
            liftIO $ wxppCacheSaveUserInfo cache app_id open_id qres
            return qres
    where
        app_id = accessTokenApp atk
