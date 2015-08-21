{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Misc where

import ClassyPrelude hiding (try)
import qualified Data.Map.Strict            as Map
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.StateVar              as SV
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import Control.Concurrent                   (threadDelay)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe
import Control.Monad.Logger
import Control.Monad.Catch
import Data.Byteable                        (toBytes)
import Yesod.Form                           (checkMMap, Field, textField, FormMessage)
import Yesod.Core                           (HandlerSite)
import Text.Shakespeare.I18N                (RenderMessage)

import Yesod.Helpers.Logger                 (LoggingTRunner(..))
import Yesod.Helpers.Persist

import Database.Persist.Sql

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.CS
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Yesod.Site.Function

import Data.Aeson
import Data.Aeson.Types                     (Parser)


aesKeyField :: forall m. (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m AesKey
aesKeyField = checkMMap conv conv_back textField
    where
        conv t = return $ either (Left . T.pack) Right $ decodeBase64AesKey t
        conv_back = fromString . C8.unpack . B64.encode . toBytes . unAesKey


-- | 从字典中找出形如 "wxpp~xxxx" 的字段，每个那样的字段解释出一个 WxppAppConfig
parseMultWxppAppConfig :: Object -> Parser (Map WxppAppID WxppAppConfig)
parseMultWxppAppConfig obj = do
    liftM (Map.fromList . catMaybes) $
        forM (HM.toList obj) $ \(k, v) -> do
            if T.isPrefixOf "wxpp~" k
                then Just . (wxppAppConfigAppID &&& id) <$> parseJSON v
                else return Nothing


type InMsgHandlerList m = [SomeWxppInMsgHandler m]

-- | 用于生成 yesod 的 subsite 类型的辅助工具
-- 它就是为了辅助制造一个 App -> WxppAppID -> MaybeWxppSub 这样的函数
mkMaybeWxppSub ::
    ( LoggingTRunner app
    , DBActionRunner app
    , DBAction app ~ SqlPersistT
    , WxppCacheBackend c
    , n ~ ResourceT (LoggingT IO)
    ) =>
    app
    -> c
    -> Maybe (IORef (Maybe (InMsgHandlerList n)))
    -> Map WxppAppID WxppAppConfig
    -> [WxppInMsgHandlerPrototype n]
    -> ([(WxppOpenID, WxppOutMsg)] -> IO ())
    -> [SomeWxppInMsgProcMiddleware n]
    -> WxppSubsiteOpts
    -> WxppAppID
    -> MaybeWxppSub
mkMaybeWxppSub foundation cache get_last_handlers_ref wxpp_config_map get_protos send_msg middlewares opts app_id =
    mkMaybeWxppSub'
        (liftM Right . runLoggingTWith foundation . runResourceT)
        foundation
        cache
        (return $ get_last_handlers_ref)
        (return $ Map.lookup app_id wxpp_config_map)
        (return $ get_protos)
        send_msg
        middlewares
        opts


mkMaybeWxppSub' ::
    ( LoggingTRunner app
    , DBActionRunner app
    , DBAction app ~ SqlPersistT
    , WxppCacheBackend c
    , SV.HasSetter hvar (Maybe (InMsgHandlerList m)), SV.HasGetter hvar (Maybe (InMsgHandlerList m))
    , MonadIO m, MonadLogger m
    ) =>
    (forall a. m a -> IO (Either String a))
    -> app
    -> c
    -> IO (Maybe hvar)
            -- ^ 用于记录上次成功配置的，可用的，消息处理规则列表
    -> IO (Maybe WxppAppConfig)
            -- ^ 根据 app id 找到相应配置的函数
    -> IO [WxppInMsgHandlerPrototype m]
    -> ([(WxppOpenID, WxppOutMsg)] -> IO ())
    -> [SomeWxppInMsgProcMiddleware m]
    -> WxppSubsiteOpts
    -> MaybeWxppSub
mkMaybeWxppSub' m_to_io foundation cache get_last_handlers_ref get_wxpp_config get_protos send_msg middlewares opts =
    MaybeWxppSub $ runMaybeT $ do
        wac <- MaybeT $ get_wxpp_config
        let data_dirs   = wxppAppConfigDataDir wac
        return $ WxppSub
                    wac
                    (SomeWxppCacheBackend cache)
                    (runDBWith foundation)
                    send_msg
                    (\x1 x2 -> liftM join $ m_to_io $ handle_msg data_dirs x1 x2)
                    (\x1 x2 -> m_to_io $ preProcessInMsgByMiddlewares middlewares cache x1 x2)
                    (runLoggingTWith foundation)
                    opts
    where
        handle_msg data_dirs bs ime = do
            err_or_in_msg_handlers <- liftIO $ do
                    protos <- get_protos
                    readWxppInMsgHandlers
                        protos
                        data_dirs
                        (T.unpack "msg-handlers.yml")

            m_last_handlers_ref <- liftIO $ get_last_handlers_ref
            m_in_msg_handlers <- case err_or_in_msg_handlers of
                Left err -> do
                    $logErrorS wxppLogSource $ fromString $
                        "cannot parse msg-handlers.yml: " ++ show err
                    m_cached_handlers <- liftIO $ fmap join $
                                            mapM SV.get m_last_handlers_ref
                    case m_cached_handlers of
                        Nothing -> do
                            $logWarnS wxppLogSource $
                                "no cached message handlers available"
                            return Nothing

                        Just x -> do
                            $logDebugS wxppLogSource $
                                "using last message handlers"
                            return $ Just x

                Right x -> return $ Just x

            case m_in_msg_handlers of
                Nothing -> do
                    return $ Left "msg-handlers.yml error"

                Just in_msg_handlers -> do
                    liftIO $ mapM_ (SV.$= Just in_msg_handlers) m_last_handlers_ref

                    -- 这里可以选择使用 tryEveryInMsgHandler'
                    -- 那样就会所有 handler 保证处理一次
                    -- tryEveryInMsgHandler'
                    tryInMsgHandlerUntilFirstPrimary'
                            cache
                            in_msg_handlers
                            bs ime


defaultInMsgProcMiddlewares :: forall m. (MonadIO m, MonadLogger m, MonadCatch m, Functor m) =>
    WxppSubDBActionRunner m
    -> WxppAppID
    -> (WxppInMsgRecordId -> WxppBriefMediaID -> IO ())
    -> [SomeWxppInMsgProcMiddleware m]
defaultInMsgProcMiddlewares db_runner app_id down_media =
    [
    SomeWxppInMsgProcMiddleware $
        (StoreInMsgToDB app_id
            db_runner
            (\x y -> liftIO $ down_media x y)
        :: StoreInMsgToDB m
        )

    , SomeWxppInMsgProcMiddleware $
        (CacheAppOpenIdToUnionId
            app_id
            db_runner
        )
    ]
    -- where db_runner = WxppSubDBActionRunner $ runDBWith foundation


-- | 如果要计算的操作有异常，记日志并重试
-- 注意：重试如果失败，还会不断重试
-- 所以只适合用于 f 本身就是一个大循环的情况
logWxppWsExcAndRetryLoop :: (MonadLogger m, MonadCatch m, MonadIO m) =>
    String      -- ^ 仅用作日志标识
    -> m ()     -- ^ 一个长时间的操作，正常返回代表工作完成
    -> m ()
logWxppWsExcAndRetryLoop op_name f = go
    where
        go = logWxppWsExcThen op_name (const $ liftIO (threadDelay 5000000) >> go) (const $ return ()) f


logWxppWsExcThen :: (MonadLogger m, MonadCatch m) =>
    String
    -> (SomeException -> m a)
                        -- ^ retry when error
    -> (b -> m a)       -- ^ next to do when ok
    -> m b              -- ^ original op
    -> m a
logWxppWsExcThen op_name on_err on_ok f = do
    err_or <- try $ tryWxppWsResult f
    case err_or of
        Left err -> do
            $logErrorS wxppLogSource $ fromString $
                op_name <> " failed: " <> show (err :: IOError)
            on_err $ toException err

        Right (Left err) -> do
            $logErrorS wxppLogSource $ fromString $
                op_name <> " failed: " <> show err
            on_err $ toException err

        Right (Right x) -> on_ok x


-- | 这个函数用于创建一个长期运行的线程
loopCleanupTimedOutForwardUrl :: (MonadIO m, MonadLogger m, MonadCatch m) =>
    (WxppAppID -> IO (Maybe AccessToken))
    -> MVar ForwardUrlMap
    -> m ()
loopCleanupTimedOutForwardUrl get_atk mvar = go
    where
        go = do
            now <- liftIO getCurrentTime
            m2 <- liftIO $ modifyMVar mvar $
                        return . Map.partition ((> now) . fst . snd)

            forM_ (Map.toList m2) $ \((open_id, app_id), (_, (_, txt))) -> do
                let outmsg = WxppOutMsgText txt

                logWxppWsExcThen "loopCleanupTimedOutForwardUrl" (const $ return ()) (const $ return ()) $ do
                    m_atk <- liftIO $ get_atk app_id
                    case m_atk of
                        Nothing -> do
                            $logErrorS wxppLogSource  "no access token available"

                        Just atk -> do
                            wxppCsSendOutMsg2 atk Nothing open_id outmsg

            liftIO $ threadDelay $ 1000 * 1000
            go
