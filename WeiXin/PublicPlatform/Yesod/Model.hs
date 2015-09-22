{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Yesod.Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi


import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Class


wxppSubModelsDef ::
#if MIN_VERSION_persistent(2, 0, 0)
    [EntityDef]
#else
    [EntityDef SqlType]
#endif
wxppSubModelsDef = $(persistFileWith lowerCaseSettings "models")

share [mkPersist sqlSettings, mkMigrate "migrateAllWxppSubModels"]
                    $(persistFileWith lowerCaseSettings "models")


type WxppDbBackend = PersistEntityBackend WxppInMsgRecord

newtype WxppDbRunner = WxppDbRunner {
                                runWxppDB ::
                                    forall a m. (MonadIO m, MonadBaseControl IO m) =>
                                        ReaderT WxppDbBackend m a -> m a
                                }

instance WxppCacheBackend WxppDbRunner where
    wxppCacheGetAccessToken (WxppDbRunner run_db) app_id = do
        run_db $ do
            fmap
                (fmap $
                    ((flip AccessToken app_id . wxppCachedAccessTokenData) &&& wxppCachedAccessTokenExpiryTime) . entityVal)
                $
                selectFirst
                    [ WxppCachedAccessTokenApp ==. app_id ]
                    [ Desc WxppCachedAccessTokenCreatedTime ]

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

    wxppCacheLookupUserInfo (WxppDbRunner run_db) app_id open_id = do
        run_db $ do
            fmap
                (fmap $
                    (fromWxppCachedUserInfoExt &&& wxppCachedUserInfoExtCreatedTime) .
                    entityVal
                ) $
                getBy $ UniqueWxppCachedUserInfoExt open_id app_id

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

