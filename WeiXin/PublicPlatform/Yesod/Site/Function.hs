{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module WeiXin.PublicPlatform.Yesod.Site.Function
    ( module WeiXin.PublicPlatform.Yesod.Site.Function
    , module WeiXin.PublicPlatform.Yesod.Site.Data
    )where

import ClassyPrelude
import Yesod
import Control.Lens
import Network.Wreq
import qualified Data.ByteString.Lazy       as LB

import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Media
import WeiXin.PublicPlatform.WS
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
                        SqlPersistT (ResourceT m) a -> m a
                    )
#endif
        }

-- | 保存所有收到的比较原始的消息（解密之后的结果）到数据库
data StoreInMsgToDB m = StoreInMsgToDB
                            (WxppSubDBActionRunner m)
                                -- ^ function to run DB actions
                            (WxppInMsgRecordId -> WxppMediaID -> m ())
                                -- ^ function to download media file
                                -- 推荐使用异步方式下载

type instance WxppInMsgProcessResult (StoreInMsgToDB m) = Maybe WxppOutMsg

instance JsonConfigable (StoreInMsgToDB m) where
    type JsonConfigableUnconfigData (StoreInMsgToDB m) =
            ( WxppSubDBActionRunner m
            , WxppInMsgRecordId -> WxppMediaID -> m ()
            )

    isNameOfInMsgHandler _ = ( == "db-store-all" )

    parseWithExtraData _ (x,y) _obj = return $ StoreInMsgToDB x y


instance (MonadIO m
#if !MIN_VERSION_persistent(2, 0, 0)
    , MonadBaseControl IO m
    , MonadLogger m
    , MonadThrow m
#endif
    ) => IsWxppInMsgProcessor m (StoreInMsgToDB m) where

    processInMsg (StoreInMsgToDB db_runner media_downloader) _acid _get_atk bs m_ime = do
        now <- liftIO getCurrentTime
        (msg_record_id, mids) <- runWxppSubDBActionRunner db_runner $ do
            let m_to        = fmap wxppInToUserName m_ime
                m_from      = fmap wxppInFromUserName m_ime
                m_ctime     = fmap wxppInCreatedTime m_ime
                m_msg_id    = join $ fmap wxppInMessageID m_ime
            msg_record_id <- insert $ WxppInMsgRecord
                            m_to m_from m_ctime m_msg_id
                            (LB.toStrict bs)
                            now

            -- save any temporary media data
            mids <- liftM (fromMaybe []) $ forM m_ime $ \ime -> do
                        case wxppInMessage ime of
                            WxppInMsgImage mid _   -> return [mid]
                            WxppInMsgVoice mid _ _ -> return [mid]
                            WxppInMsgVideo mid mid2 -> return [mid, mid2]
                            _                       -> return []
            return (msg_record_id, mids)

        forM_ mids $ \mid -> do
            media_downloader msg_record_id mid

        return $ Right Nothing


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
    AccessToken
    -> WxppInMsgRecordId
    -> WxppMediaID
#if MIN_VERSION_persistent(2, 0, 0)
    -> ReaderT backend m ()
#else
    -> m ()
#endif
downloadSaveMediaToDB atk msg_id media_id = do
    err_or_rb <- tryWxppWsResult $ wxppDownloadMedia atk media_id
    case err_or_rb of
        Left err -> do
                    $(logError) $ "Failed to download media '" <> unWxppMediaID media_id
                                    <> "': " <> (fromString $ show err)
        Right rb -> do
                    now <- liftIO getCurrentTime
                    old_or_id <- insertBy $ WxppStoredMedia
                                                media_id
                                                msg_id
                                                (LB.toStrict $ rb ^. responseBody)
                                                (rb ^. responseHeader "Content-Type")
                                                now
                    case old_or_id of
                        Left (Entity old_id _) -> do
                            $(logWarn) $ "Media '" <> unWxppMediaID media_id
                                            <> "' already in DB, record id: "
                                            <> toPathPiece old_id
                        Right _ -> return ()
