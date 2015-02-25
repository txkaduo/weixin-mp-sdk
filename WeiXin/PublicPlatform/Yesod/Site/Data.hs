{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module WeiXin.PublicPlatform.Yesod.Site.Data where

import ClassyPrelude
import Yesod
import Database.Persist.Quasi
import Database.Persist.Sql
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy       as LB

import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.InMsgHandler


data WxppSub =
        WxppSub {
                wxppSubAppConfig        :: WxppAppConfig
                    -- ^ 所有配置信息
                , wxppSubAccessTokens   :: IO (Maybe AccessToken)
                    -- ^ a computation to get usable access token
                , wxppSubMsgHandler     :: WxppInMsgHandler (LoggingT IO)
                , wxppSubRunLoggingT    :: forall a. LoggingT IO a -> IO a
                }

instance Show WxppSub where
    show (WxppSub app_config _ _ _) =
        "WxppSub: " ++ show app_config

mkYesodSubData "WxppSub" [parseRoutes|
/msg            MessageR        GET POST
/menu/reload    ReloadMenuR     GET
/menu/query     QueryMenuR      GET
|]

wxppSubModelsDef ::
#if MIN_VERSION_persistent(2, 0, 0)
    [EntityDef]
#else
    [EntityDef SqlType]
#endif
wxppSubModelsDef = $(persistFileWith lowerCaseSettings "models")

share [mkPersist sqlSettings, mkMigrate "migrateAllWxppSubModels"]
                    $(persistFileWith lowerCaseSettings "models")

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

instance JsonConfigable (StoreInMsgToDB m) where
    type JsonConfigableUnconfigData (StoreInMsgToDB m) =
            ( WxppSubDBActionRunner m
            , WxppInMsgRecordId -> WxppMediaID -> m ()
            )

    isNameOfInMsgHandler _ = ( == "db-store-all" )

    parseInMsgHandler _ _obj = return $ uncurry StoreInMsgToDB


instance (MonadIO m
#if !MIN_VERSION_persistent(2, 0, 0)
    , MonadBaseControl IO m
    , MonadLogger m
    , MonadThrow m
#endif
    ) => IsWxppInMsgHandler m (StoreInMsgToDB m) where
    handleInMsg (StoreInMsgToDB db_runner media_downloader) _acid _get_atk bs m_ime = do
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
            mids <- fmap (fromMaybe []) $ forM m_ime $ \ime -> do
                        case wxppInMessage ime of
                            WxppInMsgImage mid _   -> return [mid]
                            WxppInMsgVoice mid _ _ -> return [mid]
                            WxppInMsgVideo mid mid2 -> return [mid, mid2]
                            _                       -> return []
            return (msg_record_id, mids)

        forM_ mids $ \mid -> do
            media_downloader msg_record_id mid

        return $ Right Nothing
