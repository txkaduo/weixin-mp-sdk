module WeiXin.PublicPlatform.EndUser
    ( wxppQueryEndUserInfo
    , GetUserResult(..)
    , wxppOpenIdListInGetUserResult
    , wxppGetEndUserSource
    , wxppLookupAllCacheForUnionID
    , wxppCachedGetEndUserUnionID
    , wxppCachedQueryEndUserInfo
    , GroupBasicInfo(..)
    , wxppListUserGroups
    , wxppCreateUserGroup
    , wxppDeleteUserGroup
    , wxppRenameUserGroup
    , wxppGetGroupOfUser
    , wxppSetUserGroup
    , wxppBatchSetUserGroup
    ) where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Control.Monad.Trans.Maybe            (runMaybeT, MaybeT(..))
import Data.Aeson
import Data.Conduit                         (Source, yield)
import Data.Time                            (diffUTCTime, NominalDiffTime)
import qualified Data.Text                  as T

import WeiXin.PublicPlatform.Class
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
                        Int             -- total
                        Int             -- count
                        [WxppOpenID]
                        (Maybe WxppOpenID)
                                -- next open id


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


-- | 只找 cache: 根据 app id, open_id 找 union id
-- 因现在有不止一个 cache 表, 这个函数能找的都找
wxppLookupAllCacheForUnionID :: ( MonadIO m, WxppCacheTemp c)
                             => c
                             -> WxppAppID
                             -> WxppOpenID
                             -> m (Maybe WxppUnionID)
wxppLookupAllCacheForUnionID cache app_id open_id =
  runMaybeT $ asum [ by_sns, by_normal ]
  where
    by_sns = do
      info <- fmap fst $ MaybeT $ liftIO $ wxppCacheGetSnsUserInfo cache app_id open_id "zh_CN"
      MaybeT $ return $ oauthUserInfoUnionID info

    by_normal = do
      info <- fmap fst $ MaybeT $ liftIO $ wxppCacheLookupUserInfo cache app_id open_id
      MaybeT $ return $ endUserQueryResultUnionID info


-- | 取用户的 UnionID
-- 先从缓存找，找不到或找到的记录太旧，则调用接口
-- 如果调用接口取得最新的数据，立刻缓存之
wxppCachedGetEndUserUnionID ::
    ( MonadIO m, MonadLogger m, MonadThrow m, WxppCacheTemp c) =>
    c
    -> NominalDiffTime
    -> AccessToken
    -> WxppOpenID
    -> m (Maybe WxppUnionID)
wxppCachedGetEndUserUnionID cache ttl atk open_id = do
    liftM endUserQueryResultUnionID $ wxppCachedQueryEndUserInfo cache ttl atk open_id

wxppCachedQueryEndUserInfo ::
    (MonadIO m, MonadLogger m, MonadThrow m, WxppCacheTemp c) =>
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
            liftIO $ wxppCacheSaveUserInfo cache app_id qres
            return qres
    where
        app_id = accessTokenApp atk


data GroupBasicInfo = GroupBasicInfo
                        WxppUserGroupID
                        Text
                        Int

instance FromJSON GroupBasicInfo where
    parseJSON = withObject "GroupBasicInfo" $ \o ->
                    GroupBasicInfo <$> o .: "id"
                                    <*> o .: "name"
                                    <*> o .: "count"

instance ToJSON GroupBasicInfo where
    toJSON (GroupBasicInfo group_id name cnt) = object
                                                    [ "id"      .= group_id
                                                    , "name"    .= name
                                                    , "count"   .= cnt
                                                    ]

data ListGroupResult = ListGroupResult { unListGroupResult :: [GroupBasicInfo] }

instance FromJSON ListGroupResult where
    parseJSON = withObject "ListGroupResult" $ \o ->
                    ListGroupResult <$> o .: "groups"

-- | 取所有分组的基本信息
wxppListUserGroups ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> m [GroupBasicInfo]
wxppListUserGroups (AccessToken { accessTokenData = atk }) = do
    let url = wxppRemoteApiBaseUrl <> "/groups/get"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ getWith opts url)
                >>= asWxppWsResponseNormal'
                >>= return . unListGroupResult


data CreateGroupResult = CreateGroupResult WxppUserGroupID Text

instance FromJSON CreateGroupResult where
    parseJSON = withObject "CreateGroupResult" $ \o -> do
                    o2 <- o .: "group"
                    CreateGroupResult <$> o2 .: "id"
                                      <*> o2 .: "name"


-- | 取所有分组的基本信息
wxppCreateUserGroup ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> Text -> m WxppUserGroupID
wxppCreateUserGroup (AccessToken { accessTokenData = atk }) name = do
    let url = wxppRemoteApiBaseUrl <> "/groups/create"
        opts = defaults & param "access_token" .~ [ atk ]
    CreateGroupResult grp_id name' <-
        (liftIO $ postWith opts url $ object [ "group" .= object [ "name" .= name ] ])
                >>= asWxppWsResponseNormal'
    when (name' /= name) $ do
        $logErrorS wxppLogSource $ "creating user group but get different name: "
                    <> "expecting " <> name
                    <> ", but got " <> name'
        throwM $ userError "unexpected group name returned"
    return grp_id


-- | 删除一个用户分组
wxppDeleteUserGroup ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> WxppUserGroupID -> m ()
wxppDeleteUserGroup (AccessToken { accessTokenData = atk }) grp_id = do
    let url = wxppRemoteApiBaseUrl <> "/groups/delete"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object [ "group" .= object [ "id" .= grp_id ] ])
            >>= asWxppWsResponseVoid


-- | 修改分组名
wxppRenameUserGroup ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> WxppUserGroupID -> Text -> m ()
wxppRenameUserGroup (AccessToken { accessTokenData = atk }) grp_id name = do
    let url = wxppRemoteApiBaseUrl <> "/groups/update"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object [ "group" .= object [ "id" .= grp_id, "name" .= name ] ])
            >>= asWxppWsResponseVoid


data GetGroupResult = GetGroupResult WxppUserGroupID

instance FromJSON GetGroupResult where
    parseJSON = withObject "GetGroupResult" $ \o -> do
                    GetGroupResult <$> o .: "groupid"

-- | 查询用户所在分组
wxppGetGroupOfUser ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> WxppOpenID -> m WxppUserGroupID
wxppGetGroupOfUser (AccessToken { accessTokenData = atk }) open_id = do
    let url = wxppRemoteApiBaseUrl <> "/groups/getid"
        opts = defaults & param "access_token" .~ [ atk ]
    GetGroupResult grp_id <-
        (liftIO $ postWith opts url $ object [ "openid" .= open_id ])
            >>= asWxppWsResponseNormal'
    return grp_id


-- | 移动用户至指定分组
wxppSetUserGroup ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> WxppUserGroupID -> WxppOpenID -> m ()
wxppSetUserGroup (AccessToken { accessTokenData = atk }) grp_id open_id = do
    let url = wxppRemoteApiBaseUrl <> "/groups/members/update"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object [ "to_groupid" .= grp_id, "openid" .= open_id ])
            >>= asWxppWsResponseVoid


-- | 批量移动用户至指定分组
wxppBatchSetUserGroup ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> WxppUserGroupID -> [WxppOpenID] -> m ()
wxppBatchSetUserGroup (AccessToken { accessTokenData = atk }) grp_id open_id_list = do
    let url = wxppRemoteApiBaseUrl <> "/groups/members/batchupdate"
        opts = defaults & param "access_token" .~ [ atk ]
    (liftIO $ postWith opts url $ object [ "to_groupid" .= grp_id, "openid_list" .= open_id_list ])
            >>= asWxppWsResponseVoid
