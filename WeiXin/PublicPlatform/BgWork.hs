module WeiXin.PublicPlatform.BgWork where

import ClassyPrelude hiding (catch)
import Control.Monad.Catch                  ( catch )
import Data.Time                            (addUTCTime, NominalDiffTime)
import Control.Monad.Logger
import System.Timeout                       (timeout)
import Control.Exception                    (evaluate)

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.JS


-- | 检查最新的 access token 是否已接近过期
-- 如是，则向服务器请求更新
refreshAccessTokenIfNeeded ::
    (MonadIO m, MonadLogger m, MonadCatch m, WxppCacheBackend c) =>
    WxppAppConfig
    -> c
    -> NominalDiffTime
    -> m ()
refreshAccessTokenIfNeeded wac cache dt = do
    now <- liftIO getCurrentTime
    let t = addUTCTime (abs dt) now
    expired <- liftM (fromMaybe True . fmap ((<= t) . snd)) $
                        liftIO $ wxppCacheGetAccessToken cache app_id
    when (expired) $ do
        wxppAcquireAndSaveAccessToken cache app_id secret

    where
        app_id = wxppConfigAppID wac
        secret = wxppConfigAppSecret wac



-- | infinite loop to refresh access token
-- Create a backgroup thread to call this, so that access token can be keep fresh.
loopRefreshAccessToken ::
    (MonadIO m, MonadLogger m, MonadCatch m, WxppCacheBackend c) =>
    IO Bool     -- ^ This function should be a blocking op,
                -- return True if the infinite should be aborted.
    -> Int      -- ^ interval between successive checking (in seconds)
    -> WxppAppConfig
    -> c
    -> NominalDiffTime
    -> m ()
loopRefreshAccessToken chk_abort intv wac cache dt = do
    loopRunBgJob chk_abort intv $ refreshAccessTokenIfNeeded wac cache dt

-- | like loopRefreshAccessToken, but for a list of Weixin App
loopRefreshAccessTokens ::
    (MonadIO m, MonadLogger m, MonadCatch m, WxppCacheBackend c) =>
    IO Bool     -- ^ This function should be a blocking op,
                -- return True if the infinite should be aborted.
    -> Int      -- ^ interval between successive checking (in seconds)
    -> m [WxppAppConfig]
    -> c
    -> NominalDiffTime
    -> m ()
loopRefreshAccessTokens chk_abort intv get_wac_list cache dt = do
    loopRunBgJob chk_abort intv $ do
        wac_list <- get_wac_list
        forM_ wac_list $ \wac -> refreshAccessTokenIfNeeded wac cache dt

-- | 检查最新的 JsTicket 是否已接近过期
-- 如是，则向服务器请求更新
refreshJsTicketIfNeeded ::
    (MonadIO m, MonadLogger m, MonadCatch m, WxppCacheBackend c) =>
    c
    -> WxppAppID
    -> NominalDiffTime
    -> m ()
refreshJsTicketIfNeeded cache app_id dt = do
    now <- liftIO getCurrentTime
    m_atk_info <- wxppGetUsableAccessToken cache app_id
    case m_atk_info of
        Nothing -> do
            $logErrorS wxppLogSource $
                "cannot refresh js ticket because no access token is available, app_id=" <> unWxppAppID app_id

        Just (atk, _) -> do
            let t = addUTCTime (abs dt) now
            expired <- liftM (fromMaybe True . fmap ((<= t) . snd)) $
                                liftIO $ wxppCacheGetJsTicket cache app_id
            when (expired) $ do
                JsTicketResult ticket ttl <- wxppGetJsTicket atk
                let expiry = addUTCTime ttl now
                liftIO $ wxppCacheAddJsTicket cache app_id ticket expiry
                $logDebugS wxppLogSource $
                    "JS ticket refreshed, app_id=" <> unWxppAppID app_id

-- | infinite loop to refresh access token
-- Create a backgroup thread to call this, so that access token can be keep fresh.
loopRefreshJsTicket ::
    (MonadIO m, MonadLogger m, MonadCatch m, WxppCacheBackend c) =>
    IO Bool     -- ^ This function should be a blocking op,
                -- return True if the infinite should be aborted.
    -> Int      -- ^ interval between successive checking (in seconds)
    -> c
    -> WxppAppID
    -> NominalDiffTime
    -> m ()
loopRefreshJsTicket chk_abort intv cache app_id dt = do
    loopRunBgJob chk_abort intv $ refreshJsTicketIfNeeded cache app_id dt

-- | like loopRefreshJsTicket, but for a list of Weixin App
loopRefreshJsTickets ::
    (MonadIO m, MonadLogger m, MonadCatch m, WxppCacheBackend c) =>
    IO Bool     -- ^ This function should be a blocking op,
                -- return True if the infinite should be aborted.
    -> Int      -- ^ interval between successive checking (in seconds)
    -> c
    -> m [WxppAppID]
    -> NominalDiffTime
    -> m ()
loopRefreshJsTickets chk_abort intv cache get_app_list dt = do
    loopRunBgJob chk_abort intv $ do
        app_list <- get_app_list
        forM_ app_list $ \app_id -> refreshJsTicketIfNeeded cache app_id dt


loopRunBgJob :: (MonadIO m, MonadCatch m) =>
    IO Bool     -- ^ This function should be a blocking op,
                -- return True if the infinite should be aborted immediately.
    -> Int      -- ^ interval in seconds
    -> m ()     -- ^ the job to be repeatly called
    -> m ()
loopRunBgJob chk_abort intv job = loop
    where
        loop = do
            job >>= liftIO . evaluate
            m_if_abort <- liftIO $ timeout (intv * 1000 * 1000) chk_abort
            case m_if_abort of
                Just True   -> return ()
                _           -> loop


-- | 重复执行计算，并记录出现的异常
runRepeatlyLogExc ::
    (MonadIO m, MonadLogger m, MonadCatch m) =>
    MVar a
    -> Int      -- ^ ms
    -> m () -> m ()
runRepeatlyLogExc exit_mvar interval f = go
    where
        go = do
            f `catch` h
            liftIO (timeout interval $ readMVar exit_mvar)
                >>= maybe go (const $ return ())
        h e = do
            $(logErrorS) wxppLogSource $
                "Got exception in loop: "
                    <> fromString (show (e :: SomeException))
