{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Yesod.Model where

import ClassyPrelude.Yesod
import qualified Data.Conduit.List          as CL
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Set                   as Set
import Database.Persist.Quasi
import Database.Persist.Sql                 (Migration)
import Control.Monad.Trans.Maybe            (MaybeT(..))
import Crypto.Hash.TX.Utils                 (SHA256Hash(..))


import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.Message        (wxppInMsgEntityFromLbs)
import WeiXin.PublicPlatform.ThirdParty
import Yesod.Helpers.Persist                (insertOrReplace)


wxppSubModelsDefBasic ::
#if MIN_VERSION_persistent(2, 0, 0)
    [EntityDef]
#else
    [EntityDef SqlType]
#endif
wxppSubModelsDefBasic = $(persistFileWith lowerCaseSettings "models")

share [mkPersist sqlSettings, mkMigrate "migrateAllWxppSubModelsBasic"]
                    $(persistFileWith lowerCaseSettings "models")

wxppSubModelsDefCache ::
#if MIN_VERSION_persistent(2, 0, 0)
    [EntityDef]
#else
    [EntityDef SqlType]
#endif
wxppSubModelsDefCache = $(persistFileWith lowerCaseSettings "models_cache")

share [mkPersist sqlSettings, mkMigrate "migrateAllWxppSubModelsCache"]
                    $(persistFileWith lowerCaseSettings "models_cache")

migrateAllWxppSubModels :: Migration
migrateAllWxppSubModels = migrateAllWxppSubModelsBasic >> migrateAllWxppSubModelsCache


type WxppDbBackend = PersistEntityBackend WxppInMsgRecord

newtype WxppDbRunner = WxppDbRunner {
                                runWxppDB ::
                                    forall a m. (MonadIO m, MonadBaseControl IO m) =>
                                        ReaderT WxppDbBackend m a -> m a
                                }

instance WxppCacheTokenUpdater WxppDbRunner where
    wxppCacheAddAccessToken (WxppDbRunner run_db) atk expiry = do
        now <- liftIO getCurrentTime
        run_db $ do
            insert_ $ WxppCachedAccessToken
                            (accessTokenApp atk)
                            (accessTokenData atk)
                            expiry
                            now

    wxppCachePurgeAccessToken (WxppDbRunner run_db) expiry = do
        run_db $ do
            deleteWhere [ WxppCachedAccessTokenExpiryTime <=. expiry ]

    wxppCacheAddJsTicket (WxppDbRunner run_db) app_id (WxppJsTicket tk) expiry = do
        now <- liftIO getCurrentTime
        run_db $ do
            void $ upsert
                    (WxppCachedJsTicket app_id tk expiry now)
                    [ WxppCachedJsTicketData =. tk
                    , WxppCachedJsTicketExpiryTime =. expiry
                    , WxppCachedJsTicketCreatedTime =. now
                    ]

    wxppCachePurgeJsTicket (WxppDbRunner run_db) expiry = do
        run_db $ do
            deleteWhere [ WxppCachedJsTicketExpiryTime <=. expiry ]


instance WxppCacheTokenReader WxppDbRunner where
    wxppCacheGetAccessToken (WxppDbRunner run_db) app_id = do
        run_db $ do
            fmap
                (fmap $
                    ((flip AccessToken app_id . wxppCachedAccessTokenData) &&& wxppCachedAccessTokenExpiryTime) . entityVal)
                $
                selectFirst
                    [ WxppCachedAccessTokenApp ==. app_id ]
                    [ Desc WxppCachedAccessTokenCreatedTime ]

    wxppCacheGetJsTicket (WxppDbRunner run_db) app_id = do
        now <- liftIO getCurrentTime
        run_db $ do
            liftM (fmap $ ((WxppJsTicket . wxppCachedJsTicketData) &&& wxppCachedJsTicketExpiryTime) . entityVal) $
                selectFirst
                    [ WxppCachedJsTicketApp ==. app_id
                    , WxppCachedJsTicketExpiryTime >. now
                    ]
                    [ Desc WxppCachedJsTicketId ]


instance WxppCacheTemp WxppDbRunner where
    wxppCacheGetSnsUserInfo (WxppDbRunner run_db) app_id open_id lang = do
        runMaybeT $ do
            rec <- liftM entityVal $ MaybeT $ run_db $
                        getBy $ UniqueWxppCachedSnsUserInfo app_id open_id lang
            let info = OAuthGetUserInfoResult
                        open_id
                        (wxppCachedSnsUserInfoNickname rec)
                        (wxppCachedSnsUserInfoGender rec)
                        (wxppCachedSnsUserInfoCountry rec)
                        (wxppCachedSnsUserInfoProvince rec)
                        (wxppCachedSnsUserInfoCity rec)
                        (wxppCachedSnsUserInfoHeadImgUrl rec)
                        (wxppCachedSnsUserInfoPrivileges rec)
                        (wxppCachedSnsUserInfoUnionId rec)
            return (info, wxppCachedSnsUserInfoUpdatedTime rec)

    wxppCacheAddSnsUserInfo (WxppDbRunner run_db) app_id lang info = do
        now <- liftIO getCurrentTime
        let rec = WxppCachedSnsUserInfo
                    app_id
                    open_id
                    (oauthUserInfoUnionID info)
                    lang
                    (oauthUserInfoGender info)
                    (oauthUserInfoNickname info)
                    (oauthUserInfoCountry info)
                    (oauthUserInfoProvince info)
                    (oauthUserInfoCity info)
                    (oauthUserInfoHeadImgUrl info)
                    (oauthUserInfoPrivileges info)
                    now
        run_db $ do
            -- 因为这个表有两个 unique ，就不用 upsert 了
            m_old_id <- liftM (fmap entityKey) $ getBy $ UniqueWxppCachedSnsUserInfo app_id open_id lang
            case m_old_id of
                Nothing -> insert_ rec
                Just old_id -> do
                    update old_id
                        [ WxppCachedSnsUserInfoUnionId =. oauthUserInfoUnionID info
                        , WxppCachedSnsUserInfoGender =. oauthUserInfoGender info
                        , WxppCachedSnsUserInfoNickname =. oauthUserInfoNickname info
                        , WxppCachedSnsUserInfoCountry =. oauthUserInfoCountry info
                        , WxppCachedSnsUserInfoProvince =. oauthUserInfoProvince info
                        , WxppCachedSnsUserInfoCity =. oauthUserInfoCity info
                        , WxppCachedSnsUserInfoHeadImgUrl =. oauthUserInfoHeadImgUrl info
                        , WxppCachedSnsUserInfoPrivileges =. oauthUserInfoPrivileges info
                        , WxppCachedSnsUserInfoUpdatedTime =. now
                        ]
        where
            open_id = oauthUserInfoOpenID info


    wxppCacheLookupUserInfo (WxppDbRunner run_db) app_id open_id = do
        run_db $ do
            fmap
                (fmap $
                    (fromWxppCachedUserInfoExt &&& wxppCachedUserInfoExtCreatedTime) .
                    entityVal
                ) $
                getBy $ UniqueWxppCachedUserInfoExt open_id app_id

    wxppCacheLookupAllOpenIdByUid (WxppDbRunner run_db) uid = do
        run_db $ do
          -- 因为现在有多个cache了用户open_id/union_id的表
          -- 目前大部分都有更新(依赖具体使用sdk程序的逻辑),
          -- 但不排除以后为优化而去掉其中某个表
          -- 这里用一种简单粗暴的方式: 能找的都找了
          list0 <- by_table_WxppUserCachedInfo
          let app_id_list0 = map fst list0

          -- WxppCachedUserInfoExt 是经常更新的
          list1 <- fmap
                    (fmap $
                        (wxppCachedUserInfoExtApp &&& wxppCachedUserInfoExtOpenId) .
                        entityVal
                    )
                    (selectList [ WxppCachedUserInfoExtUnionId ==. Just uid
                                , WxppCachedUserInfoExtApp /<-. app_id_list0
                                ]
                               [])

          let app_id_list1 = map fst $ list0 <> list1

          list2<- fmap
                    (fmap $
                        (wxppCachedSnsUserInfoApp &&& wxppCachedSnsUserInfoOpenId) .
                        entityVal
                    )
                    (selectList [ WxppCachedSnsUserInfoUnionId ==. Just uid
                                , WxppCachedSnsUserInfoApp /<-. app_id_list1
                                ]
                                [])

          return $ fmap (uncurry WxppAppOpenID) $ ordNub $ list0 <> list1 <> list2

        where
          by_table_WxppUserCachedInfo = do
            -- 这个表的结构就是为这种反查而做的,效率最高,先找它
            fmap
                (fmap $
                    (wxppUserCachedInfoApp &&& wxppUserCachedInfoOpenId) .
                    entityVal
                )
                (selectList [ WxppUserCachedInfoUnionId ==. Just uid ] [])

    wxppCacheSaveUserInfo (WxppDbRunner run_db) app_id qres = do
        now <- liftIO getCurrentTime
        run_db $ do
            case toWxppCachedUserInfoExt app_id now qres of
                Nothing -> do
                    -- 用户不再关注
                    -- 暂时不删除缓存
                    return ()

                Just rec -> do
                    insertBy rec
                        >>= either (flip replace rec . entityKey) (const $ return ())

    wxppCacheLookupUploadedMediaIDByHash (WxppDbRunner run_db) app_id h = do
        run_db $ do
            m_rec <- getBy $ UniqueWxppCachedUploadedMediaHash h app_id
            case entityVal <$> m_rec of
                Nothing -> return Nothing
                Just (WxppCachedUploadedMedia _app_id _md5 mtype mid ctime) -> do
                    return $ Just $ UploadResult mtype mid ctime

    wxppCacheSaveUploadedMediaID
        (WxppDbRunner run_db) app_id h (UploadResult mtype mid ctime) = do
        run_db $ do
            let rec = WxppCachedUploadedMedia app_id h mtype mid ctime
            insertBy rec >>= either (flip replace rec . entityKey) (const $ return ())

    wxppCacheGetOAuthAccessToken (WxppDbRunner run_db) app_id open_id req_scopes = do
        now <- liftIO getCurrentTime
        runResourceT $ run_db $ do
            selectSource
                    [ WxppCachedOAuthTokenApp ==. app_id
                    , WxppCachedOAuthTokenOpenId ==. open_id
                    , WxppCachedOAuthTokenExpiryTime >. now
                    ]
                    [ Desc WxppCachedOAuthTokenId ]
                =$= check_scopes
                $$ CL.head
        where
            check_scopes = awaitForever $ \(Entity rec_id rec) -> do
                has_scopes <- lift $
                                liftM (Set.fromList .
                                        (map $  wxppCachedOAuthTokenScopeScope . entityVal)
                                    ) $
                                    selectList [ WxppCachedOAuthTokenScopeToken ==. rec_id ] []
                when ( Set.isSubsetOf req_scopes has_scopes ) $ do
                    yield $ mk_p rec has_scopes

            mk_p rec scopes = OAuthTokenInfo
                                (wxppCachedOAuthTokenAccess rec)
                                (wxppCachedOAuthTokenRefresh rec)
                                scopes
                                (wxppCachedOAuthTokenExpiryTime rec)

    wxppCacheAddOAuthAccessToken (WxppDbRunner run_db) atk_p expiry = do
        now <- liftIO getCurrentTime
        run_db $ do
            rec_id <- insert $ WxppCachedOAuthToken app_id open_id atk rtk expiry now
            insertMany_ $
                map (\x -> WxppCachedOAuthTokenScope rec_id x) $ toList scopes
        where
            app_id    = oauthAtkPAppID atk_p
            open_id   = oauthAtkPOpenID atk_p
            atk       = oauthAtkPRaw atk_p
            rtk       = oauthAtkPRtk atk_p
            scopes    = oauthAtkPScopes atk_p

    wxppCachePurgeOAuthAccessToken (WxppDbRunner run_db) expiry = do
        run_db $
            deleteWhere [ WxppCachedOAuthTokenExpiryTime <=. expiry ]


fromWxppCachedUserInfoExt :: WxppCachedUserInfoExt -> EndUserQueryResult
fromWxppCachedUserInfoExt
    (WxppCachedUserInfoExt
        _app_id
        open_id
        m_union_id
        nickname
        m_gender
        locale
        city
        province
        country
        head_img
        subs_time
        _create_time)
    = EndUserQueryResult
        open_id
        nickname
        m_gender
        locale
        city
        province
        country
        head_img
        subs_time
        m_union_id

toWxppCachedUserInfoExt :: WxppAppID -> UTCTime -> EndUserQueryResult -> Maybe WxppCachedUserInfoExt
toWxppCachedUserInfoExt _ _ (EndUserQueryResultNotSubscribed {}) = Nothing
toWxppCachedUserInfoExt app_id created_time
    (EndUserQueryResult
        open_id
        nickname
        m_gender
        locale
        city
        province
        country
        head_img
        subs_time
        m_union_id)
    = Just $ WxppCachedUserInfoExt
        app_id
        open_id
        m_union_id
        nickname
        m_gender
        locale
        city
        province
        country
        head_img
        subs_time
        created_time


-- | Read history from WxppInMsgRecord table, generate WxppInMsgEntity
wxppSourceInMsgEntityFromHistory :: ( MonadResource m
#if MIN_VERSION_persistent(2, 5, 0)
                                    , MonadReader backend m, HasPersistBackend backend, BaseBackend backend ~ SqlBackend
#else
                                    , MonadReader env m, HasPersistBackend env SqlBackend
#endif
                                    )
                                 => [Filter WxppInMsgRecord]
                                 -> [SelectOpt WxppInMsgRecord]
                                 -> Source m (Either String (UTCTime, (Maybe WxppAppID, WxppInMsgEntity)))

wxppSourceInMsgEntityFromHistory filters opts = do
  selectSource filters opts
    =$= CL.map parse_ime
  where
    parse_ime (Entity _ rec) = do
      ime <- wxppInMsgEntityFromLbs $ LB.fromStrict $ wxppInMsgRecordBlob rec
      let app_id = wxppInMsgRecordApp rec
      return (wxppInMsgRecordNotifyTime rec, (app_id, ime))



instance WxppTpTokenReader WxppDbRunner where
  wxppTpTokenGetVeriyTicket (WxppDbRunner run_db) app_id = do
    run_db $ do
      fmap (fmap wxppCachedTpCompVerifyTicketTicket) $ get $ WxppCachedTpCompVerifyTicketKey app_id


  wxppTpTokenGetComponentAccessToken (WxppDbRunner run_db) app_id = do
    runMaybeT $ do
      fmap (get_res . entityVal) $
            MaybeT $ run_db $ do
              selectFirst
                [ WxppCachedTpCompAccessTokenApp ==. app_id
                ]
                [ Desc WxppCachedTpCompAccessTokenExpiryTime ]
    where
      get_res = (flip WxppTpAccessToken app_id . wxppCachedTpCompAccessTokenData)
                  &&& wxppCachedTpCompAccessTokenExpiryTime


  wxppTpTokenGetAuthorizerTokens (WxppDbRunner run_db) comp_app_id auther_app_id = do
    runMaybeT $ do
      fmap (to_tokens . entityVal) $
            MaybeT $ run_db $ do
              selectFirst
                [ WxppCachedTpAutherTokenComponentApp ==. comp_app_id
                , WxppCachedTpAutherTokenAutherApp ==. auther_app_id
                ]
                [ Desc WxppCachedTpAutherTokenExpiryTime ]

    where
      to_tokens x = WxppTpAuthorizerTokens
                      (flip AccessToken auther_app_id $ wxppCachedTpAutherTokenAccess x)
                      (flip WxppTpRefreshToken auther_app_id $ wxppCachedTpAutherTokenRefresh x)
                      (wxppCachedTpAutherTokenExpiryTime x)


  wxppTpTokenSourceAuthorizerTokens (WxppDbRunner run_db) = do
    transPipe run_db $
      mapOutput ((wxppCachedTpAutherTokenComponentApp &&& get_res) . entityVal) $ selectSource [] []
    where
      to_tokens auther_app_id x = WxppTpAuthorizerTokens
                                    (flip AccessToken auther_app_id $ wxppCachedTpAutherTokenAccess x)
                                    (flip WxppTpRefreshToken auther_app_id $ wxppCachedTpAutherTokenRefresh x)
                                    (wxppCachedTpAutherTokenExpiryTime x)

      get_res rec = let auther_app_id = wxppCachedTpAutherTokenAutherApp rec
                     in to_tokens auther_app_id rec



instance WxppTpTokenWriter WxppDbRunner where
  wxppTpTokenSaveVerifyTicket (WxppDbRunner run_db) app_id ticket = do
    now <- getCurrentTime
    run_db $ do
      void $ insertOrReplace $ WxppCachedTpCompVerifyTicket app_id ticket now


  wxppTpTokenDeleteVerifyTicket (WxppDbRunner run_db) app_id = do
    run_db $ do
      delete $ WxppCachedTpCompVerifyTicketKey app_id

  wxppTpTokenAddComponentAccessToken (WxppDbRunner run_db) (WxppTpAccessToken raw_atk app_id) expiry = do
    now <- getCurrentTime
    run_db $ do
      insert_ $ WxppCachedTpCompAccessToken app_id raw_atk expiry now

  wxppTpTokenPurgeComponentAccessToken (WxppDbRunner run_db) app_id expiry = do
    run_db $ do
      deleteWhere [ WxppCachedTpCompAccessTokenApp ==. app_id
                  , WxppCachedTpCompAccessTokenExpiryTime <. expiry
                  ]


  wxppTpTokenAddAuthorizerTokens (WxppDbRunner run_db) comp_app_id (WxppTpAuthorizerTokens access_token refresh_token expiry) = do
    when (app_id /= app_id2) $ do
      error $ "access token and refresh token have different app ids"

    now <- getCurrentTime
    run_db $ do
      insert_ $ do
        WxppCachedTpAutherToken
          comp_app_id app_id raw_atk raw_rtk expiry now

    where
      AccessToken raw_atk app_id = access_token
      WxppTpRefreshToken raw_rtk app_id2 = refresh_token


  wxppTpTokenPurgeAuthorizerTokens (WxppDbRunner run_db) comp_app_id m_auth_app_id m_expiry = do
    run_db $ do
      deleteWhere $
        catMaybes
          [ Just (WxppCachedTpAutherTokenComponentApp ==. comp_app_id)
          , fmap (WxppCachedTpAutherTokenExpiryTime <.) m_expiry
          , fmap (WxppCachedTpAutherTokenAutherApp ==.) m_auth_app_id
          ]
