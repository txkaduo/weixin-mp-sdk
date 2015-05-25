{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module WeiXin.PublicPlatform.Misc where

import ClassyPrelude hiding (try, FilePath, (<.>), (</>))
import qualified Data.Map.Strict            as Map
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import Filesystem.Path.CurrentOS            (encodeString, fromText, (</>))
import Control.Concurrent                   (threadDelay)
import Control.Monad.Logger
import Control.Monad.Catch

import Yesod.Helpers.Utils                  (LoggingTRunner(..))
import Yesod.Helpers.Persist

import Database.Persist.Sql

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.CS
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Yesod.Site.Function

import Data.Aeson
import Data.Aeson.Types                     (Parser)


-- | 从字典中找出形如 "wxpp~xxxx" 的字段，每个那样的字段解释出一个 WxppAppConfig
parseMultWxppAppConfig :: Object -> Parser (Map WxppAppID WxppAppConfig)
parseMultWxppAppConfig obj = do
    liftM (Map.fromList . catMaybes) $
        forM (HM.toList obj) $ \(k, v) -> do
            if T.isPrefixOf "wxpp~" k
                then Just . (wxppAppConfigAppID &&& id) <$> parseJSON v
                else return Nothing


-- | 用于生成 yesod 的 subsite 类型的辅助工具
-- 它就是为了辅助制造一个 App -> WxppAppID -> MaybeWxppSub 这样的函数
mkMaybeWxppSub ::
    ( LoggingTRunner app
    , DBActionRunner app
    , DBAction app ~ SqlPersistT
    , WxppCacheBackend c
    ) =>
    app
    -> c
    -> Map WxppAppID WxppAppConfig
    -> (WxppAppID -> [WxppInMsgHandlerPrototype (LoggingT IO)])
    -> Chan (WxppAppID, [WxppOutMsgEntity])
    -> Chan (WxppAppID, (WxppInMsgRecordId, WxppMediaID))
    -> WxppSubsiteOpts
    -> WxppAppID
    -> MaybeWxppSub
mkMaybeWxppSub foundation cache wxpp_config_map get_protos send_msg_ch down_media_ch opts app_id =
    MaybeWxppSub $ case Map.lookup app_id wxpp_config_map of
        Nothing     -> Nothing
        Just wac    ->  let data_dir    = wxppAppConfigDataDir wac
                        in Just $ WxppSub wac
                                    (SomeWxppCacheBackend cache)
                                    (runDBWith foundation)
                                    send_msg
                                    (handle_msg data_dir)
                                    middlewares
                                    (runLoggingTWith foundation)
                                    opts
    where
        db_runner = WxppSubDBActionRunner $ runDBWith foundation

        -- middlewares :: [SomeWxppInMsgProcMiddleware (LoggingT IO)]
        middlewares =
                [
                SomeWxppInMsgProcMiddleware $
                    (StoreInMsgToDB app_id
                        db_runner
                        (\x y -> liftIO $ writeChan down_media_ch (app_id, (x, y)))
                    :: StoreInMsgToDB (LoggingT IO)
                    )

                , SomeWxppInMsgProcMiddleware $
                    (CacheAppOpenIdToUnionId
                        app_id
                        db_runner
                    )
                ]

        handle_msg data_dir bs ime = do
            err_or_in_msg_handlers <- liftIO $
                    readWxppInMsgHandlers
                        (get_protos app_id)
                        (encodeString $ data_dir </> fromText "msg-handlers.yml")

            case err_or_in_msg_handlers of
                Left err -> do
                    $logErrorS wxppLogSource $ fromString $
                        "cannot parse msg-handlers.yml: " ++ show err
                    return $ Left "msg-handlers.yml error"

                Right in_msg_handlers -> do
                    -- 这里可以选择使用 tryEveryInMsgHandler'
                    -- 那样就会所有 handler 保证处理一次
                    -- tryEveryInMsgHandler'
                    tryInMsgHandlerUntilFirstPrimary'
                            cache
                            in_msg_handlers
                            bs ime

        send_msg msgs = writeChan send_msg_ch (app_id, msgs)


-- | 如果要计算的操作有异常，记日志并重试
-- 注意：重试如果失败，还会不断重试
-- 所以只适合用于 f 本身就是一个大循环的情况
logWxppWsExcAndRetryLoop :: (MonadLogger m, MonadCatch m) =>
    String      -- ^ 仅用作日志标识
    -> m ()     -- ^ 一个长时间的操作，正常返回代表工作完成
    -> m ()
logWxppWsExcAndRetryLoop op_name f = go
    where
        go = logWxppWsExcThen op_name (const go) (const $ return ()) f


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
                let outmsg_e = WxppOutMsgEntity open_id
                                (error "wxppOutFromUserName forced in loopCheckAndPrompt")
                                now
                                (WxppOutMsgText txt)

                logWxppWsExcThen "loopCheckAndPrompt" (const $ return ()) (const $ return ()) $ do
                    m_atk <- liftIO $ get_atk app_id
                    case m_atk of
                        Nothing -> do
                            $logErrorS wxppLogSource  "no access token available"

                        Just atk -> do
                                (wxppCsSendOutMsg atk Nothing outmsg_e)

            liftIO $ threadDelay $ 1000 * 1000
            go