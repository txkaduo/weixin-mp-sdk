{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.EndUser
    ( wxppQueryEndUserInfo
    , wxppBatchQueryEndUserInfo, wxppBatchQueryEndUserInfoMaxNum
    , GetUserResult(..)
    , wxppOpenIdListInGetUserResult
    , wxppGetEndUserSource
    , wxppGetEndUserSource'
    , wxppLookupAllCacheForUnionID
    , wxppCachedGetEndUserUnionID
    , wxppCachedQueryEndUserInfo
    , wxppCachedBatchQueryEndUserInfo
    , GroupBasicInfo(..)
    , wxppListUserGroups
    , wxppCreateUserGroup
    , wxppDeleteUserGroup
    , wxppRenameUserGroup
    , wxppGetGroupOfUser
    , wxppSetUserGroup
    , wxppBatchSetUserGroup
    ) where

import ClassyPrelude hiding ((\\))
import Network.Wreq hiding (Proxy)
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Control.Monad.Reader                 (asks)
import Control.Monad.Trans.Maybe            (runMaybeT, MaybeT(..))
import Data.Aeson
import qualified Data.Aeson.Extra           as AE
import Data.Conduit                         (Source, yield)
import Data.Time                            (diffUTCTime, NominalDiffTime)
import Data.List                            ((\\))
import Data.Proxy

import Yesod.Helpers.Utils                  (nullToNothing)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS


-- | 调用服务器接口，查询用户基础信息
wxppQueryEndUserInfo :: (WxppApiMonad env m)
                     => AccessToken
                     -> WxppOpenID
                     -> m EndUserQueryResult
wxppQueryEndUserInfo (AccessToken { accessTokenData = atk }) (WxppOpenID open_id) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/user/info"
        opts = defaults & param "access_token" .~ [ atk ]
                        & param "openid" .~ [ open_id ]
                        & param "lang" .~ [ "zh_CN" :: Text ]

    liftIO (WS.getWith opts sess url)
                >>= asWxppWsResponseNormal'


-- | 调用服务器接口，批量查询用户基础信息
wxppBatchQueryEndUserInfo :: (WxppApiMonad env m)
                          => AccessToken
                          -> [WxppOpenID]
                          -> m (Map WxppOpenID EndUserQueryResult)
wxppBatchQueryEndUserInfo (AccessToken { accessTokenData = atk }) open_ids = do
  (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
  let url = wxppUrlConfSecureApiBase url_conf <> "/user/info/batchget"
      opts = defaults & param "access_token" .~ [ atk ]
                      & param "lang" .~ [ "zh_CN" :: Text ]

  liftIO (WS.postWith opts sess url $ object [ "user_list" .= map to_jv open_ids ])
    >>= asWxppWsResponseNormal'
    >>= return . AE.getSingObject (Proxy :: Proxy "user_info_list")
    >>= return . mapFromList . map (getWxppOpenID &&& id)

  where to_jv open_id = object [ "lang" .= asText "zh_CN"
                               , "openid" .= open_id
                               ]


wxppBatchQueryEndUserInfoMaxNum :: Int
wxppBatchQueryEndUserInfoMaxNum = 100

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

                    -- 平台是用空字串表示结束的
                    next_openid <- fmap WxppOpenID . join . fmap nullToNothing <$> obj .:? "next_openid"
                    return $ GetUserResult
                                total count lst next_openid

wxppOpenIdListInGetUserResult :: GetUserResult -> [WxppOpenID]
wxppOpenIdListInGetUserResult (GetUserResult _ _ x _) = x

-- | 调用服务器接口，查询所有订阅用户
wxppGetEndUserSource' :: (WxppApiMonad env m)
                     => m AccessToken
                     -- ^ 我们要反复取用 access token,　而且不确定用多长时间
                     -> Source m GetUserResult
wxppGetEndUserSource' get_atk = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)

    let url         = wxppUrlConfSecureApiBase url_conf <> "/user/get"
        loop m_start_id = do
            AccessToken { accessTokenData = atk } <- lift get_atk
            let opts = defaults & param "access_token" .~ [ atk ]
                                & (case m_start_id of
                                    Nothing     -> id
                                    Just (WxppOpenID start_open_id) ->
                                        param "next_openid" .~ [ start_open_id ]
                                    )

            r@(GetUserResult _ _ _ m_next_id) <-
                liftIO (WS.getWith opts sess url) >>= asWxppWsResponseNormal'
            yield r

            maybe (return ()) (loop . Just) m_next_id

    loop Nothing


{-# DEPRECATED  wxppGetEndUserSource "use wxppGetEndUserSource' instead" #-}
wxppGetEndUserSource :: (WxppApiMonad env m)
                     => AccessToken
                     -> Source m GetUserResult
wxppGetEndUserSource = wxppGetEndUserSource' . return


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
    ( WxppApiMonad env m, WxppCacheTemp c) =>
    c
    -> NominalDiffTime
    -> AccessToken
    -> WxppOpenID
    -> m (Maybe WxppUnionID)
wxppCachedGetEndUserUnionID cache ttl atk open_id = do
    liftM endUserQueryResultUnionID $ wxppCachedQueryEndUserInfo cache ttl atk open_id


-- | 取用户信息，优先查询cache
wxppCachedQueryEndUserInfo :: (WxppApiMonad env m, WxppCacheTemp c)
                           => c
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
    where app_id = accessTokenApp atk


-- | 批量取用户信息，优先查询cache
wxppCachedBatchQueryEndUserInfo :: (WxppApiMonad env m, WxppCacheTemp c)
                                => c
                                -> NominalDiffTime
                                -> AccessToken
                                -> [WxppOpenID]
                                -> m (Map WxppOpenID EndUserQueryResult)
wxppCachedBatchQueryEndUserInfo cache ttl atk open_ids = do
  cached_results <- fmap catMaybes $
    forM open_ids $ \ open_id -> do
      m_res <- liftIO $ wxppCacheLookupUserInfo cache app_id open_id
      now <- liftIO getCurrentTime
      return $ case m_res of
                  Just ((EndUserQueryResultNotSubscribed {}), _) ->
                      -- never do negative cache
                      Nothing

                  Just (qres, ctime) ->
                      if diffUTCTime now ctime > ttl
                          then Nothing
                          else Just (open_id, qres)

                  Nothing -> Nothing

  let cached_open_ids = map fst cached_results
  let uncached_open_ids = open_ids \\ cached_open_ids

  let split_inputs results xs =
        let (xs1, xs2) = splitAt wxppBatchQueryEndUserInfoMaxNum xs
         in if null xs1
               then reverse results
               else split_inputs (xs1 : results) xs2

  uncached_results <-
    fmap mconcat $ forM (split_inputs [] uncached_open_ids) $ \ to_query_open_ids -> do
      results <- wxppBatchQueryEndUserInfo atk to_query_open_ids

      unless (length results == length to_query_open_ids) $ do
        $logWarnS wxppLogSource $ "wxppBatchQueryEndUserInfo return unmatched result num: got "
          <> tshow (length results)
          <> ", but expected " <> tshow (length to_query_open_ids)

      return results

  let cached_results' = mapFromList cached_results :: Map WxppOpenID EndUserQueryResult

  fmap (mapFromList . catMaybes) $
    forM open_ids $ \ open_id -> do
      case lookup open_id cached_results' of
        Just qres -> return $ Just (open_id, qres)
        Nothing -> do
          case lookup open_id uncached_results of
            Just qres -> do
              liftIO $ wxppCacheSaveUserInfo cache app_id qres
              return $ Just (open_id, qres)

            Nothing -> do
              $logWarnS wxppLogSource $ "failed to find end user info for: " <> unWxppOpenID open_id
              return Nothing

  where app_id = accessTokenApp atk


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
wxppListUserGroups :: (WxppApiMonad env m) => AccessToken -> m [GroupBasicInfo]
wxppListUserGroups (AccessToken { accessTokenData = atk }) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/groups/get"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.getWith opts sess url)
                >>= asWxppWsResponseNormal'
                >>= return . unListGroupResult


data CreateGroupResult = CreateGroupResult WxppUserGroupID Text

instance FromJSON CreateGroupResult where
    parseJSON = withObject "CreateGroupResult" $ \o -> do
                    o2 <- o .: "group"
                    CreateGroupResult <$> o2 .: "id"
                                      <*> o2 .: "name"


-- | 取所有分组的基本信息
wxppCreateUserGroup :: (WxppApiMonad env m)
                    => AccessToken
                    -> Text
                    -> m WxppUserGroupID
wxppCreateUserGroup (AccessToken { accessTokenData = atk }) name = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/groups/create"
        opts = defaults & param "access_token" .~ [ atk ]

    CreateGroupResult grp_id name' <-
        liftIO (WS.postWith opts sess url $ object [ "group" .= object [ "name" .= name ] ])
                >>= asWxppWsResponseNormal'
    when (name' /= name) $ do
        $logErrorS wxppLogSource $ "creating user group but get different name: "
                    <> "expecting " <> name
                    <> ", but got " <> name'
        throwM $ userError "unexpected group name returned"
    return grp_id


-- | 删除一个用户分组
wxppDeleteUserGroup :: (WxppApiMonad env m)
                    => AccessToken
                    -> WxppUserGroupID
                    -> m ()
wxppDeleteUserGroup (AccessToken { accessTokenData = atk }) grp_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/groups/delete"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "group" .= object [ "id" .= grp_id ] ])
            >>= asWxppWsResponseVoid


-- | 修改分组名
wxppRenameUserGroup :: (WxppApiMonad env m)
                    => AccessToken
                    -> WxppUserGroupID
                    -> Text
                    -> m ()
wxppRenameUserGroup (AccessToken { accessTokenData = atk }) grp_id name = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/groups/update"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "group" .= object [ "id" .= grp_id, "name" .= name ] ])
            >>= asWxppWsResponseVoid


data GetGroupResult = GetGroupResult WxppUserGroupID

instance FromJSON GetGroupResult where
    parseJSON = withObject "GetGroupResult" $ \o -> do
                    GetGroupResult <$> o .: "groupid"

-- | 查询用户所在分组
wxppGetGroupOfUser :: (WxppApiMonad env m)
                   => AccessToken
                   -> WxppOpenID
                   -> m WxppUserGroupID
wxppGetGroupOfUser (AccessToken { accessTokenData = atk }) open_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/groups/getid"
        opts = defaults & param "access_token" .~ [ atk ]

    GetGroupResult grp_id <-
        liftIO (WS.postWith opts sess url $ object [ "openid" .= open_id ])
            >>= asWxppWsResponseNormal'
    return grp_id


-- | 移动用户至指定分组
wxppSetUserGroup :: (WxppApiMonad env m)
                 => AccessToken
                 -> WxppUserGroupID
                 -> WxppOpenID
                 -> m ()
wxppSetUserGroup (AccessToken { accessTokenData = atk }) grp_id open_id = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/groups/members/update"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "to_groupid" .= grp_id, "openid" .= open_id ])
            >>= asWxppWsResponseVoid


-- | 批量移动用户至指定分组
wxppBatchSetUserGroup :: (WxppApiMonad env m)
                      => AccessToken
                      -> WxppUserGroupID
                      -> [WxppOpenID]
                      -> m ()
wxppBatchSetUserGroup (AccessToken { accessTokenData = atk }) grp_id open_id_list = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/groups/members/batchupdate"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ object [ "to_groupid" .= grp_id, "openid_list" .= open_id_list ])
            >>= asWxppWsResponseVoid
