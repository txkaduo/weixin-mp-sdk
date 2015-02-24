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
                        , PersistEntityBackend WxppIncomingRawMsg ~ backend
                        , PersistEntityBackend WxppIncomingHeader ~ backend
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

-- | 保存所有收到的信息到数据库
data StoreMessageToDB m = StoreMessageToDB (WxppSubDBActionRunner m)

instance JsonConfigable (StoreMessageToDB m) where
    type JsonConfigableUnconfigData (StoreMessageToDB m) = WxppSubDBActionRunner m

    isNameOfInMsgHandler _ = ( == "db-store-all" )

    parseInMsgHandler _ _obj = return StoreMessageToDB


instance (MonadIO m
#if !MIN_VERSION_persistent(2, 0, 0)
    , MonadBaseControl IO m
    , MonadLogger m
    , MonadThrow m
#endif
    ) => IsWxppInMsgHandler m (StoreMessageToDB m) where
    handleInMsg (StoreMessageToDB db_runner) _acid _get_atk bs m_ime = do
        now <- liftIO getCurrentTime
        runWxppSubDBActionRunner db_runner $ do
            m_header_id <- forM m_ime $ \ime -> do
                            insert $ WxppIncomingHeader
                                        (wxppInToUserName ime)
                                        (wxppInFromUserName ime)
                                        (wxppInCreatedTime ime)
                                        (wxppInMessageID ime)
                                        now
            insert_ $ WxppIncomingRawMsg
                            m_header_id
                            (LB.toStrict bs)
        return $ Right Nothing
