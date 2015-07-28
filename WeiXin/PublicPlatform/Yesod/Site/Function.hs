{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module WeiXin.PublicPlatform.Yesod.Site.Function
    ( module WeiXin.PublicPlatform.Yesod.Site.Function
    , module WeiXin.PublicPlatform.Yesod.Site.Data
    )where

import ClassyPrelude
import Yesod
import Control.Lens
import Network.Wreq
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Conduit.List          as CL
import Data.Conduit
import Database.Persist.Sql

import Yesod.Helpers.Persist

import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Media
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.EndUser
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.Yesod.Site.Data


newtype WxppSubDBActionRunner m = WxppSubDBActionRunner
        {
            runWxppSubDBActionRunner ::
#if MIN_VERSION_persistent(2, 0, 0)
                    (forall backend a.
                        ( PersistStore backend
                        , PersistEntityBackend WxppInMsgRecord ~ backend
                        , PersistEntityBackend WxppStoredMedia ~ backend
                        ) =>
                        ReaderT backend m a -> m a
                    )
#else
                    -- XXX: 这里没有找到一种可以中立于数据库backend的表达方法
                    -- 暂时只能写死是 SqlPersistT
                    -- 这种写法不支持 MongoDB
                    (forall a .
                        SqlPersistT m a -> m a
                    )
#endif
        }

-- | Handler: 保存所有收到的比较原始的消息（解密之后的结果）到数据库
data StoreInMsgToDB m = StoreInMsgToDB
                            WxppAppID
                            (WxppSubDBActionRunner m)
                                -- function to run DB actions
                            (WxppInMsgRecordId -> WxppBriefMediaID -> m ())
                                -- function to download media file
                                -- 推荐使用异步方式下载

type instance WxppInMsgProcessResult (StoreInMsgToDB m) = WxppInMsgHandlerResult

instance JsonConfigable (StoreInMsgToDB m) where
    type JsonConfigableUnconfigData (StoreInMsgToDB m) =
            ( WxppAppID
            , WxppSubDBActionRunner m
            , WxppInMsgRecordId -> WxppBriefMediaID -> m ()
            )

    isNameOfInMsgHandler _ = ( == "db-store-all" )

    parseWithExtraData _ (x,y,z) _obj = return $ StoreInMsgToDB x y z


instance (MonadIO m, MonadLogger m
#if !MIN_VERSION_persistent(2, 0, 0)
    , MonadBaseControl IO m
    , MonadLogger m
    , MonadThrow m
#endif
    ) => IsWxppInMsgProcessor m (StoreInMsgToDB m) where

    processInMsg (StoreInMsgToDB {}) _cache _bs _m_ime = do
        $logWarnS wxppLogSource $
            "StoreInMsgToDB now do nothing when used as incoming message handler"
        return $ Right []


instance (MonadIO m, MonadLogger m
#if !MIN_VERSION_persistent(2, 0, 0)
    , MonadBaseControl IO m
    , MonadLogger m
    , MonadThrow m
#endif
    ) => IsWxppInMsgProcMiddleware m (StoreInMsgToDB m) where
    preProcInMsg (StoreInMsgToDB app_id db_runner media_downloader) _cache bs m_ime = runMaybeT $ do
        now <- liftIO getCurrentTime
        (msg_record_id, mids) <- mapMaybeT (runWxppSubDBActionRunner db_runner) $ do
            let m_to        = fmap wxppInToUserName m_ime
                m_from      = fmap wxppInFromUserName m_ime
                m_ctime     = fmap wxppInCreatedTime m_ime
                m_msg_id    = join $ fmap wxppInMessageID m_ime
            old_or_msg_record_id <- lift $ insertBy $ WxppInMsgRecord
                            app_id
                            m_to m_from m_ctime m_msg_id
                            (LB.toStrict bs)
                            now

            msg_record_id <- case old_or_msg_record_id of
                Left (Entity old_id _) -> do
                    $logWarnS wxppLogSource $
                        "got a duplicate message from WeiXin platform: db id=" <> toPathPiece old_id
                            <>", MsgId=" <> (fromString $ show $ fmap unWxppInMsgID m_msg_id)
                    mzero

                Right x -> return x

            -- save any temporary media data
            mids <- liftM (fromMaybe []) $ forM m_ime $ \ime -> do
                        case wxppInMessage ime of
                            WxppInMsgImage mid _   -> return [mid]
                            WxppInMsgVoice mid _ _ -> return [mid]
                            WxppInMsgVideo mid mid2 -> return [mid, mid2]
                            _                       -> return []
            return (msg_record_id, mids)

        lift $ forM_ mids $ \mid -> do
            media_downloader msg_record_id mid
        return (bs, m_ime)


-- | Handler: 更新 WxppOpenIdUnionId 的记录
data CacheAppOpenIdToUnionId m = CacheAppOpenIdToUnionId
                                    WxppAppID
                                    (WxppSubDBActionRunner m)
                                        -- ^ function to run DB actions

type instance WxppInMsgProcessResult (CacheAppOpenIdToUnionId m) = WxppInMsgHandlerResult

instance JsonConfigable (CacheAppOpenIdToUnionId m) where
    type JsonConfigableUnconfigData (CacheAppOpenIdToUnionId m) =
            ( WxppAppID
            , WxppSubDBActionRunner m
            )

    isNameOfInMsgHandler _ = ( == "update-openid-to-unionid" )

    parseWithExtraData _ (x, y) _obj = return $ CacheAppOpenIdToUnionId x y


instance (MonadIO m
    , MonadCatch m
    , MonadLogger m
    , Functor m
#if !MIN_VERSION_persistent(2, 0, 0)
    , MonadBaseControl IO m
#endif
    ) => IsWxppInMsgProcessor m (CacheAppOpenIdToUnionId m) where

    processInMsg (CacheAppOpenIdToUnionId {}) _cache _bs _m_ime = runExceptT $ do
        $logWarnS wxppLogSource $
            "CacheAppOpenIdToUnionId now do nothing when used as incoming message handler"
        return []


instance (MonadIO m
    , MonadCatch m
    , MonadLogger m
    , Functor m
#if !MIN_VERSION_persistent(2, 0, 0)
    , MonadBaseControl IO m
#endif
    ) => IsWxppInMsgProcMiddleware m (CacheAppOpenIdToUnionId m) where

    preProcInMsg (CacheAppOpenIdToUnionId app_id db_runner) cache bs m_ime = do
        forM_ m_ime $ \ime -> do
            let m_subs_or_unsubs = case wxppInMessage ime of
                            (WxppInMsgEvent WxppEvtSubscribe)               -> Just True
                            (WxppInMsgEvent (WxppEvtSubscribeAtScene {}))   -> Just True
                            (WxppInMsgEvent WxppEvtUnsubscribe)             -> Just False
                            _                                               -> Nothing

            case m_subs_or_unsubs of
                Just True -> void $ runExceptT $ do
                    atk <- (tryWxppWsResultE "getting access token" $ liftIO $
                                wxppCacheGetAccessToken cache app_id)
                            >>= maybe (throwE $ "no access token available") (return . fst)
                    let open_id = wxppInFromUserName ime
                    qres <- tryWxppWsResultE "wxppQueryEndUserInfo" $
                                wxppQueryEndUserInfo atk open_id

                    let m_uid = endUserQueryResultUnionID qres
                    now <- liftIO getCurrentTime

                    lift $ runWxppSubDBActionRunner db_runner $ do
                        void $ insertOrUpdate
                            (WxppUserCachedInfo app_id open_id m_uid now)
                            [ WxppUserCachedInfoUnionId =. m_uid
                            , WxppUserCachedInfoUpdatedTime =. now
                            ]

                Just False -> do
                    -- 取消关注时，目前先不删除记录
                    -- 估计 openid unionid 对于固定的用户是固定的
                    return ()

                _ -> return ()

        return $ Just (bs, m_ime)

-- | 下载多媒体文件，保存至数据库
downloadSaveMediaToDB ::
    ( MonadLogger m
    , MonadCatch m
    , MonadIO m
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistUnique backend
    , backend ~ PersistEntityBackend WxppStoredMedia
#else
    , PersistUnique m
    , PersistMonadBackend m ~ PersistEntityBackend WxppStoredMedia
#endif
    ) =>
    Bool
    -> AccessToken
    -> WxppInMsgRecordId
    -> WxppBriefMediaID
#if MIN_VERSION_persistent(2, 0, 0)
    -> ReaderT backend m ()
#else
    -> m ()
#endif
downloadSaveMediaToDB if_ssl atk msg_id media_id = do
    err_or_rb <- tryWxppWsResult $ wxppDownloadMedia if_ssl atk media_id
    case err_or_rb of
        Left err -> do
                    $(logErrorS) wxppLogSource $ "Failed to download media '" <> unWxppBriefMediaID media_id
                                    <> "': " <> (fromString $ show err)
        Right rb -> do
                    now <- liftIO getCurrentTime
                    old_or_id <- insertBy $ WxppStoredMedia
                                                (accessTokenApp atk)
                                                media_id
                                                msg_id
                                                (LB.toStrict $ rb ^. responseBody)
                                                (rb ^. responseHeader "Content-Type")
                                                now
                    case old_or_id of
                        Left (Entity old_id _) -> do
                            $(logWarnS) wxppLogSource $ "Media '" <> unWxppBriefMediaID media_id
                                            <> "' already in DB, record id: "
                                            <> toPathPiece old_id
                        Right _ -> return ()


instance WxppCacheBackend (WxppSubDBActionRunner IO) where
    wxppCacheGetAccessToken (WxppSubDBActionRunner run_db) app_id = do
        run_db $ do
            fmap
                (fmap $
                    ((flip AccessToken app_id . wxppCachedAccessTokenData) &&& wxppCachedAccessTokenExpiryTime) . entityVal)
                $
                selectFirst
                    [ WxppCachedAccessTokenApp ==. app_id ]
                    [ Desc WxppCachedAccessTokenCreatedTime ]

    wxppCacheAddAccessToken (WxppSubDBActionRunner run_db) atk expiry = do
        now <- liftIO getCurrentTime
        run_db $ do
            insert_ $ WxppCachedAccessToken
                            (accessTokenApp atk)
                            (accessTokenData atk)
                            expiry
                            now

    wxppCachePurgeAccessToken (WxppSubDBActionRunner run_db) expiry = do
        run_db $ do
            deleteWhere [ WxppCachedAccessTokenExpiryTime <=. expiry ]

    wxppCacheLookupUserInfo (WxppSubDBActionRunner run_db) app_id open_id = do
        run_db $ do
            fmap
                (fmap $
                    (fromWxppCachedUserInfoExt &&& wxppCachedUserInfoExtCreatedTime) .
                    entityVal
                ) $
                getBy $ UniqueWxppCachedUserInfoExt open_id app_id

    wxppCacheSaveUserInfo (WxppSubDBActionRunner run_db) app_id qres = do
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

    wxppCacheLookupUploadedMediaIDByHash (WxppSubDBActionRunner run_db) app_id h = do
        run_db $ do
            m_rec <- getBy $ UniqueWxppCachedUploadedMediaHash h app_id
            case entityVal <$> m_rec of
                Nothing -> return Nothing
                Just (WxppCachedUploadedMedia _app_id _md5 mtype mid ctime) -> do
                    return $ Just $ UploadResult mtype mid ctime

    wxppCacheSaveUploadedMediaID
        (WxppSubDBActionRunner run_db) app_id h (UploadResult mtype mid ctime) = do
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


-- | 找出最近一段时间内有消息发給系统的用户
wxppUserLatestActiveTime :: (MonadIO m, MonadResource m) =>
    UTCTime         -- ^ 只检查过去一段时间内的消息历史
    -> WxppAppID
    -> Source (SqlPersistT m) (WxppOpenID, UTCTime)
wxppUserLatestActiveTime start_time app_id = do
    open_id_fn <- lift $ getFieldName WxppInMsgRecordFrom
    created_time_fn <- lift $ getFieldName WxppInMsgRecordCreatedTime
    app_fn <- lift $ getFieldName WxppInMsgRecordApp
    table_name <- lift $ getTableName (error "WxppInMsgRecord forced" :: WxppInMsgRecord)
    let query = "SELECT "
                    <> open_id_fn
                    <> ",MAX(" <> created_time_fn <> ")"
                    <> " FROM "
                    <> table_name
                    <> " WHERE "
                    <> app_fn <> "= ?"
                    <> " AND "
                    <> created_time_fn <> ">= ?"
                    <> " GROUP BY " <> open_id_fn
    rawQuery query [ toPersistValue app_id, toPersistValue start_time]
        $= CL.mapM (\x -> case x of
                        [v1, v2]    -> return $
                                            (,) <$> fromPersistValue v1
                                                <*> fromPersistValue v2

                        _       -> throwM $ PersistMarshalError $
                                            "Expecting 2 columns, but got "
                                                <> (fromString $ show $ length x)
                    )
        =$= CL.mapM (either (throwM . PersistMarshalError) return)
