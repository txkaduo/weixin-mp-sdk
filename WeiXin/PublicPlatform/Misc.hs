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
import qualified Data.ByteString            as B
import Control.Concurrent                   (threadDelay)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe
import Control.Monad.Logger
import Data.Char
import Network.Mime                         (MimeType)
import Data.Byteable                        (toBytes)
import Yesod.Form                           (checkMMap, Field, textField, FormMessage)
import Yesod.Core                           (HandlerSite, PathPiece(..))
import Text.Shakespeare.I18N                (RenderMessage)

import Yesod.Helpers.Logger                 (LoggingTRunner(..))
import Yesod.Helpers.Persist

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.CS
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Yesod.Site.Function
import WeiXin.PublicPlatform.Yesod.Model

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

-- | parseMultWxppAppConfig 相似
-- 但使用这个解释得到的字典是 wxpp~xxx 中的 xxx 作为键值（而不是app id）
-- 'xxx' 部分可以作为外部标识app的字串（例如用在 url上）
parseMultiWxppAppConfig2 :: Object -> Parser (Map Text WxppAppConfig)
parseMultiWxppAppConfig2 obj = do
    liftM (Map.fromList . catMaybes) $
        forM (HM.toList obj) $ \(k, v) -> do
            case T.stripPrefix "wxpp~" k of
                Nothing     -> return Nothing
                Just cname  -> Just . (cname, ) <$> parseJSON v

-- | 为方便在 URL 上使用 code name 而设（与 parseMultWxppAppConfig2 的结果配合使用）
newtype CodeNameOrAppID = CodeNameOrAppID { unCodeNameOrAppID :: Text }
                        deriving (Eq, Ord, Show, Read)

instance PathPiece CodeNameOrAppID where
    toPathPiece (CodeNameOrAppID x) = toPathPiece x
    fromPathPiece = fmap CodeNameOrAppID . fromPathPiece

lookupMultiWxppAppConfig2 :: CodeNameOrAppID
                            -> Map Text WxppAppConfig
                            -> Maybe WxppAppConfig
lookupMultiWxppAppConfig2 (CodeNameOrAppID t) the_map =
    Map.lookup t the_map <|> lookup_by_app_id the_map
    where
        lookup_by_app_id = fmap fst . Map.minView .
                                Map.filterWithKey
                                    (\_ wac -> wxppConfigAppID wac == WxppAppID t)

type InMsgHandlerList m = [SomeWxppInMsgHandler m]


-- | 用于构造 MaybeWxppSub Subsite 的工具
mkMaybeWxppSub ::
    ( LoggingTRunner app
    , DBActionRunner app
    , DBAction app ~ ReaderT WxppDbBackend
    , WxppCacheTokenReader c, WxppCacheTemp c
    , SV.HasSetter hvar (Maybe (InMsgHandlerList m)), SV.HasGetter hvar (Maybe (InMsgHandlerList m))
    , MonadIO m, MonadLogger m
    ) =>
    (forall a. m a -> IO (Either String a))
    -> app
    -> c
    -> (WxppAppID -> IO (Maybe hvar))
            -- ^ 用于记录上次成功配置的，可用的，消息处理规则列表
    -> IO (Maybe WxppAppConfig)
            -- ^ 找到相应配置的函数
    -> (WxppAppConfig -> IO [WxppInMsgHandlerPrototype m])
    -> (WxppAppID -> [(WxppOpenID, WxppOutMsg)] -> IO ())
    -> (WxppAppID -> IO [SomeWxppInMsgProcMiddleware m])
    -> WxppSubsiteOpts
    -> MaybeWxppSub
mkMaybeWxppSub m_to_io foundation cache get_last_handlers_ref get_wxpp_config get_protos send_msg get_middleware opts =
    MaybeWxppSub $ runMaybeT $ do
        wac <- MaybeT $ get_wxpp_config
        let app_id      = wxppConfigAppID wac
        let data_dirs   = wxppAppConfigDataDir wac
        middlewares <- liftIO $ get_middleware app_id
        return $ WxppSub
                    wac
                    (SomeWxppCacheClient cache)
                    (WxppDbRunner $ runDBWith foundation)
                    (send_msg app_id)
                    (\x1 x2 -> liftM join $ m_to_io $ handle_msg wac data_dirs x1 x2)
                    (\x1 x2 -> m_to_io $ preProcessInMsgByMiddlewares middlewares cache x1 x2)
                    (\x1 -> m_to_io $ postProcessInMsgByMiddlewares middlewares x1)
                    (runLoggingTWith foundation)
                    opts
    where
        handle_msg wac data_dirs bs ime = do
            let app_id      = wxppConfigAppID wac
            err_or_in_msg_handlers <- liftIO $ do
                    protos <- get_protos wac
                    readWxppInMsgHandlers
                        protos
                        data_dirs
                        (T.unpack "msg-handlers.yml")

            m_last_handlers_ref <- liftIO $ get_last_handlers_ref app_id
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


defaultInMsgProcMiddlewares :: forall m.
    (MonadIO m, MonadLogger m, MonadCatch m, MonadBaseControl IO m, Functor m) =>
    WxppDbRunner
    -> WxppAppID
    -> (Bool -> WxppInMsgRecordId -> WxppBriefMediaID -> IO ())
    -> [SomeWxppInMsgProcMiddleware m]
defaultInMsgProcMiddlewares db_runner app_id down_media =
    [
    SomeWxppInMsgProcMiddleware $
        (StoreInMsgToDB app_id
            db_runner
            (\x y z -> liftIO $ down_media x y z)
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
logWxppWsExcAndRetryLoop :: (MonadLogger m, MonadCatch m, MonadIO m, MonadBaseControl IO m) =>
    String      -- ^ 仅用作日志标识
    -> m ()     -- ^ 一个长时间的操作，正常返回代表工作完成
    -> m ()
logWxppWsExcAndRetryLoop op_name f = go
    where
        go = logWxppWsExcThen op_name (const $ liftIO (threadDelay 5000000) >> go) (const $ return ()) f


logWxppWsExcThen :: (MonadLogger m, MonadCatch m, MonadBaseControl IO m) =>
    String
    -> (SomeException -> m a)
                        -- ^ retry when error
    -> (b -> m a)       -- ^ next to do when ok
    -> m b              -- ^ original op
    -> m a
logWxppWsExcThen op_name on_err on_ok f = do
    err_or <- tryAny $ tryWxppWsResult f
    case err_or of
        Left err -> do
            $logErrorS wxppLogSource $ fromString $
                op_name <> " failed: " <> show err
            on_err $ toException err

        Right (Left err) -> do
            $logErrorS wxppLogSource $ fromString $
                op_name <> " failed: " <> show err
            on_err $ toException err

        Right (Right x) -> on_ok x


-- | 这个函数用于创建一个长期运行的线程
loopCleanupTimedOutForwardUrl :: (MonadIO m, MonadLogger m, MonadCatch m, MonadBaseControl IO m) =>
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


retryGetAccessTokenDo :: (MonadIO m, MonadLogger m) =>
                        Int     -- ^ microseconds interval between retries
                        -> Int  -- retry number
                        -> m (Maybe AccessToken)
                        -> (AccessToken -> m a)
                        -> m (Maybe a)
retryGetAccessTokenDo delay retry_cnt get_atk use_atk = go 0
    where
        go cnt
            | cnt > retry_cnt = return Nothing

            | otherwise = do
                m_atk <- get_atk

                case m_atk of
                    Nothing -> do
                        $logError $ "cannot get access token"
                        liftIO $ threadDelay delay
                        go (cnt + 1)

                    Just atk -> liftM Just $ use_atk atk


-- | XXX: this is a incomplete list
-- 这只是根据微信文档，把可能常用的mime映射到最常见的扩展名
fileExtOfMime :: IsString s => MimeType -> s
fileExtOfMime "text/plain"    = ".txt"
fileExtOfMime "audio/x-speex" = ".spx"
fileExtOfMime mime            =
    -- 处理所有类似于 image/jpeg -> jpeg 这样的情况
    case m_sub of
        Just sub | not (B.null main) &&
                    not (B.null sub) &&
                    B.length sub <= 4 &&
                    B.all (isAlphaNum . chr . fromIntegral) sub -> fromString $ '.' : C8.unpack sub
        _   ->  ".dat"
    where (main,m_sub) = second (stripPrefix "/") $ break (== fromIntegral (ord '/')) mime


-- | 微信发过来的语音有个 format 字段，据此生成一个合适的文件扩展名
contentTypeOfAudioFormat :: Text -> Maybe MimeType
contentTypeOfAudioFormat "amr"   = Just "audio/amr"
contentTypeOfAudioFormat "speex" = Just "audio/x-speex"
contentTypeOfAudioFormat _       = Nothing
