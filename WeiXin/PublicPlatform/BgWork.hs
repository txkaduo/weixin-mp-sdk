{-# LANGUAGE CPP #-}
module WeiXin.PublicPlatform.BgWork where

-- {{{1 imports
import ClassyPrelude
import qualified Control.Exception.Safe as ExcSafe
import Data.Time                            (addUTCTime, NominalDiffTime)
import Control.Monad.Logger

#if !MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Exception                    (evaluate)
import System.Timeout                       (timeout)
#endif

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.JS
import WeiXin.PublicPlatform.WS
-- }}}1

-- | 检查最新的 access token 是否已接近过期
-- 如是，则向服务器请求更新
refreshAccessTokenIfNeeded :: (WxppApiMonad env m, ExcSafe.MonadCatch m, WxppCacheTokenUpdater c, WxppCacheTokenReader c
                              , HasWxppAppID cf, HasWxppSecret cf)
                           => cf
                           -> c
                           -> NominalDiffTime
                           -> m ()
refreshAccessTokenIfNeeded wac cache dt = do
    refreshAccessTokenIfNeeded' cache app_id secret dt
    where
        app_id = getWxppAppID wac
        secret = getWxppSecret wac


refreshAccessTokenIfNeeded' :: ( WxppApiMonad env m, ExcSafe.MonadCatch m
                                , WxppCacheTokenUpdater c, WxppCacheTokenReader c)
                            => c
                            -> WxppAppID
                            -> WxppAppSecret
                            -> NominalDiffTime
                            -> m ()
refreshAccessTokenIfNeeded' cache app_id secret dt = do
    now <- liftIO getCurrentTime
    let t = addUTCTime (abs dt) now
    expired <- liftM (fromMaybe True . fmap ((<= t) . snd)) $
                        liftIO $ wxppCacheGetAccessToken cache app_id
    when (expired) $ do
        wxppAcquireAndSaveAccessToken cache app_id secret


-- | infinite loop to refresh access token
-- Create a backgroup thread to call this, so that access token can be keep fresh.
loopRefreshAccessToken :: (WxppApiMonad env m, ExcSafe.MonadCatch m, WxppCacheTokenUpdater c, WxppCacheTokenReader c
                         , HasWxppAppID cf, HasWxppSecret cf)
                       => IO Bool  -- ^ This function should be a blocking op,
                                   -- return True if the infinite should be aborted.
                       -> Int      -- ^ interval between successive checking (in seconds)
                       -> cf
                       -> c
                       -> NominalDiffTime
                       -> m ()
loopRefreshAccessToken chk_abort intv wac cache dt = do
    loopRunBgJob chk_abort intv $ refreshAccessTokenIfNeeded wac cache dt

-- | like loopRefreshAccessToken, but for a list of Weixin App
loopRefreshAccessTokens :: (WxppApiMonad env m, ExcSafe.MonadCatch m, WxppCacheTokenUpdater c, WxppCacheTokenReader c
                           , HasWxppAppID cf, HasWxppSecret cf)
                        => IO Bool  -- ^ This function should be a blocking op,
                                    -- return True if the infinite should be aborted.
                        -> Int      -- ^ interval between successive checking (in seconds)
                        -> m [cf]
                        -> c
                        -> NominalDiffTime
                        -> m ()
loopRefreshAccessTokens chk_abort intv get_wac_list cache dt = do
    loopRunBgJob chk_abort intv $ do
        wac_list <- get_wac_list
        forM_ wac_list $ \wac -> refreshAccessTokenIfNeeded wac cache dt

-- | 检查最新的 JsTicket 是否已接近过期
-- 如是，则向服务器请求更新
refreshJsTicketIfNeeded :: ( WxppApiMonad env m, ExcSafe.MonadCatch m, WxppCacheTokenUpdater c, WxppCacheTokenReader c)
                        => c
                        -> WxppAppID
                        -> NominalDiffTime
                        -> m ()
refreshJsTicketIfNeeded cache app_id dt = do
    ws_res <- tryWxppWsResult inner
    case ws_res of
        Left err -> do
            $(logErrorS) wxppLogSource $
                "Failed to refresh JS API ticket for app: "
                    <> unWxppAppID app_id
                    <> ", error was: "
                    <> fromString (show err)
        Right () -> return ()
    where
        inner = do
            now <- liftIO getCurrentTime
            let t = addUTCTime (abs dt) now
            expired <- liftM (fromMaybe True . fmap ((<= t) . snd)) $
                                liftIO $ wxppCacheGetJsTicket cache app_id
            when expired $ wxppAcquireAndSaveJsApiTicket cache app_id

-- | infinite loop to refresh access token
-- Create a backgroup thread to call this, so that access token can be keep fresh.
loopRefreshJsTicket :: (WxppApiMonad env m, ExcSafe.MonadCatch m, WxppCacheTokenUpdater c, WxppCacheTokenReader c)
                    => IO Bool  -- ^ This function should be a blocking op,
                                -- return True if the infinite should be aborted.
                    -> Int      -- ^ interval between successive checking (in seconds)
                    -> c
                    -> WxppAppID
                    -> NominalDiffTime
                    -> m ()
loopRefreshJsTicket chk_abort intv cache app_id dt = do
    loopRunBgJob chk_abort intv $ refreshJsTicketIfNeeded cache app_id dt

-- | like loopRefreshJsTicket, but for a list of Weixin App
loopRefreshJsTickets :: (WxppApiMonad env m, ExcSafe.MonadCatch m, WxppCacheTokenUpdater c, WxppCacheTokenReader c)
                     => IO Bool     -- ^ This function should be a blocking op,
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


loopRunBgJob :: (MonadIO m) =>
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
