{-#LANGUAGE CPP #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Menu where

-- {{{1 imports
import ClassyPrelude
import qualified Control.Exception.Safe as ExcSafe
import Network.Wreq
import qualified Network.Wreq.Session       as WS
import Control.Lens hiding ((.=))
import Control.Monad.Logger
#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Reader                 (asks)
#endif
import Data.Aeson
import Control.Monad.Trans.Except
import System.FilePath                      (takeDirectory, normalise)
import qualified System.FSNotify            as FN
import qualified Control.Concurrent.Async   as Async
import qualified Data.Map.Strict            as Map
import Control.Monad.Trans.Control
import System.Directory                     (doesFileExist, doesDirectoryExist, canonicalizePath)
import Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.List.NonEmpty         as LNE

import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Concurrent (threadDelay)
#endif
-- }}}1

-- | result in query menu
data MenuDef = MenuDef [MenuItem]

instance FromJSON MenuDef where
    parseJSON = withObject "MenuDef" $ \obj -> do
                    obj .: "menu"
                        >>= withObject "Menu" (\o -> fmap MenuDef $ o .: "button")


-- | 调用服务器接口，创建菜单
wxppCreateMenu :: (WxppApiMonad env m)
               => AccessToken
               -> [MenuItem]
               -> m ()
wxppCreateMenu (AccessToken { accessTokenData = atk }) menus = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/menu/create"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.postWith opts sess url $ toJSON $ object [ "button" .= menus ])
        >>= asWxppWsResponseVoid


-- | 调用服务器接口，查询菜单
-- 据文档，此接口只能查询到由 wxppCreateMenu 所用接口设置的菜单
-- CAUTION: 还有一个类似的接口叫 “获取自定义菜单配置接口”
wxppQueryMenu :: (WxppApiMonad env m)
              => AccessToken
            -> m [MenuItem]
wxppQueryMenu (AccessToken { accessTokenData = atk }) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/menu/get"
        opts = defaults & param "access_token" .~ [ atk ]

    MenuDef items <- liftIO (WS.getWith opts sess url)
                        >>= asWxppWsResponseNormal'
    return items


-- | 获取自定义菜单配置接口定义的返回值
data WxppMenuConfig = WxppMenuConfig {
                            wxppMenuConfigEnabled   :: Bool
                            , wxppMenuConfigItems   :: [MenuItem]
                        }

-- | 获取自定义菜单配置接口
-- XXX: 由于返回的 JSON 结构有点混乱，不同设置方式下得到的值不一样
-- 目前暂时不作进一步解释，只当作一个字典保存
wxppQueryMenuConfig :: (WxppApiMonad env m)
                    => AccessToken
                  -> m Value
wxppQueryMenuConfig (AccessToken { accessTokenData = atk }) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/get_current_selfmenu_info"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.getWith opts sess url) >>= asWxppWsResponseNormal'


-- | 调用服务器接口，删除菜单
wxppDeleteMenu :: (WxppApiMonad env m)
               => AccessToken
               -> m ()
wxppDeleteMenu (AccessToken { accessTokenData = atk }) = do
    (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
    let url = wxppUrlConfSecureApiBase url_conf <> "/menu/delete"
        opts = defaults & param "access_token" .~ [ atk ]

    liftIO (WS.getWith opts sess url)
        >>= asWxppWsResponseVoid


-- | 根据指定 YAML 文件配置调用远程接口，修改菜单
wxppCreateMenuWithYaml :: (WxppApiMonad env m, ExcSafe.MonadCatch m)
                       => AccessToken
                       -> NonEmpty FilePath
                       -> FilePath
                       -> m (Either String ())
wxppCreateMenuWithYaml access_token data_dirs fp = runExceptT $ do
    err_or_menu <- liftIO $ do
                    runDelayedYamlLoaderL data_dirs $ mkDelayedYamlLoader fp
    case err_or_menu of
        Left err    -> do
            $(logErrorS) wxppLogSource $
                "Failed to parse menu yml: " <> fromString (show err)
            throwE $ "Failed to parse yml: " <> show err
        Right menu  -> do
            err_or <- lift $ tryWxppWsResult $
                            if null menu
                                then wxppDeleteMenu access_token
                                else wxppCreateMenu access_token menu
            case err_or of
                Left err    -> do
                                $(logErrorS) wxppLogSource $
                                        "Failed to reload menu: " <> fromString (show err)
                                throwE $ "Failed to reload menu: " <> show err
                Right _     -> return ()


#if MIN_VERSION_monad_control(1, 0, 0)
#else
liftBaseOpDiscard :: MonadBaseControl b m
                  => ((a -> b ()) -> b c)
                  ->  (a -> m ()) -> m c
liftBaseOpDiscard f g = liftBaseWith $ \runInBase -> f $ void . runInBase . g
#endif

-- | 不断地监护菜单配置文件变化，自动修改微信的菜单
-- 不返回，直至 block_until_exit 参数的计算完成
wxppWatchMenuYaml :: (WxppApiMonad env m, ExcSafe.MonadMask m, MonadBaseControl IO m)
                  => m (Maybe AccessToken)
                  -> IO ()        -- ^ bloack until exit
                  -> NonEmpty FilePath
                  -> FilePath
                  -> m ()
wxppWatchMenuYaml get_atk block_until_exit data_dirs fname = do
    -- 主动加载一次菜单配置
    load

    ExcSafe.bracket (liftIO $ FN.startManagerConf watch_cfg) (liftIO . FN.stopManager) $ \mgr -> do
        fp' <- liftM catMaybes $ forM (toList fp) $ \p -> do
            err_or <- liftIO $ tryIOError $ canonicalizePath p
            case err_or of
                Left err
                    | isDoesNotExistError err -> return Nothing
                    | otherwise               -> do
                                                $logWarnS wxppLogSource $ fromString $
                                                    "canonicalizePath failed on path: " <> p
                                                    <> ", error was: " <> show err
                                                return Nothing
                Right x -> return $ Just x

        -- XXX: can only watch existing dir
        stops <- liftM (catMaybes . toList) $ forM dirs $ \dir -> do
                    let predi = const True
                    b <- liftIO $ doesDirectoryExist dir
                    if b
                        then do
                            $logDebugS wxppLogSource $ fromString $ "watching for dir: " <> dir
                            liftM Just $
                                (liftBaseOpDiscard $
                                    FN.watchDir
                                        mgr
                                        dir
                                        predi) (handle_evt fp')
                        else return Nothing

        liftIO $ block_until_exit >> sequence stops >> return ()
    where
        dirs = fmap takeDirectory fp
        -- fp' = fmap normalise fp
        fp = fmap (</> fname) data_dirs

        watch_cfg = FN.defaultConfig
#if MIN_VERSION_fsnotify(0, 4, 0)
#else
                        { FN.confDebounce = FN.Debounce (fromIntegral (1 :: Int)) }
                        -- 把时间相近(1秒内)的事件合并
                        -- 很多对文件操作的工具保存时都可能由多个文件系统操作完成
                        -- 比如 vim
#endif

        handle_evt fp' evt = do
            case evt of
                FN.Removed {} -> do
                    -- do not handle remove event
                    -- when modify file with editor like vim, file maybe removed then save again
                    -- To remove a menu, modify the menu to be empty list
                    return ()

                _ -> do
                    let evt_fp = normalise $ FN.eventPath evt
                    if evt_fp `elem` fp'
                        then do
                            -- 用 vim 在线修改文件时，总是收到一个 Removed 的事件
                            -- 干脆不理会 event 的类型，直接检查文件是否存在
                            exists <- liftIO $ threadDelay (500 * 1000) >> doesFileExist evt_fp
                            if not exists
                                then do
                                    $logWarnS wxppLogSource $
                                        "menu config file has been removed or inaccessible: "
                                            <> (fromString $ FN.eventPath evt)
                                    -- 如果打算停用菜单，可直接将配置文件清空
                                else do
                                    load
                        else do
                            --- $logDebugS wxppLogSource $ fromString $ "skipping notification for path: " <> evt_fp
                            return ()

        load = do
            m_atk <- get_atk
            case m_atk of
                Nothing             -> do
                    $logErrorS  wxppLogSource $ "Failed to create menu: no access token available."

                Just access_token   ->  do
                    let app_id = accessTokenApp access_token
                    err_or <- wxppCreateMenuWithYaml access_token data_dirs fname
                    case err_or of
                        Left err    -> $logErrorS  wxppLogSource $
                                                "Failed to create menu for app: "
                                                <> unWxppAppID app_id
                                                <> ", error was: " <> fromString err
                        Right _     -> $logInfoS wxppLogSource $
                                            "menu reloaded for app: " <> unWxppAppID app_id


-- | like wxppWatchMenuYaml, but watch for many weixin app all at once
wxppWatchMenuYamlOnSignal :: forall env m. (WxppApiMonad env m, ExcSafe.MonadMask m, MonadBaseControl IO m)
                          => IO ()        -- ^ bloack until exit
                          -> FilePath
                          -> (WxppAppID -> IO (Maybe AccessToken))
                          -> (WxppAppID -> IO (NonEmpty FilePath))
                          -> IO WxppSignal
                              -- ^ should be a blocking op
                          -> m ()
wxppWatchMenuYamlOnSignal block_until_exit fname get_atk get_data_dirs get_event = do
    watch_data <- liftIO $ newIORef Map.empty

    let add_app mgr app_id = do
            data_dirs <- liftIO $ get_data_dirs app_id
            let fp = fmap (</> fname) data_dirs
            normalized_fp <- liftM catMaybes $ forM (toList fp) $ \p -> do
                err_or <- liftIO $ tryIOError $ canonicalizePath p
                case err_or of
                    Left err
                        | isDoesNotExistError err -> return Nothing
                        | otherwise               -> do
                                                    $logWarnS wxppLogSource $ fromString $
                                                        "canonicalizePath failed on path: " <> p
                                                        <> ", error was: " <> show err
                                                    return Nothing
                    Right x -> return $ Just x

            let dirs = fmap takeDirectory normalized_fp

            -- load menu before watching for changes
            load (get_atk app_id) normalized_fp

            -- XXX: can only watch existing dir
            stops <- liftM catMaybes $ forM dirs $ \dir -> do
                        let predi = const True
                        b <- liftIO $ doesDirectoryExist dir
                        if b
                            then do
                                $logDebugS wxppLogSource $ fromString $ "watching for dir: " <> dir
                                liftM Just $
                                    (liftBaseOpDiscard $
                                        FN.watchDir
                                            mgr
                                            dir
                                            predi)
                                            (handle_evt (get_atk app_id) normalized_fp)
                            else return Nothing

            liftIO $ atomicModifyIORef' watch_data $ \the_map -> (Map.insert app_id stops the_map, ())

    let remove_app app_id = do
            stops <- liftIO $ atomicModifyIORef' watch_data $ \the_map ->
                                let stops = Map.lookup app_id the_map
                                    new_map = Map.delete app_id the_map
                                in (new_map, stops)
            void $ liftIO $ sequence $ fromMaybe [] stops

    let loop mgr = do
            exit_or_evt <- liftIO $ Async.race block_until_exit get_event
            case exit_or_evt of
                Left _ -> do
                    stops <- liftM (join . Map.elems) $ liftIO $ readIORef watch_data
                    void $ liftIO $ sequence stops
                    return ()

                Right evt -> do
                    case evt of
                        WxppSignalNewApp app_id -> add_app mgr app_id
                        WxppSignalRemoveApp app_id -> remove_app app_id
                    loop mgr

    ExcSafe.bracket (liftIO $ FN.startManagerConf watch_cfg) (liftIO . FN.stopManager) $ \mgr -> do
        loop mgr

    where
        -- dirs = fmap takeDirectory fp
        -- fp' = fmap normalise fp
        -- fp = fmap (</> fname) data_dirs

        watch_cfg = FN.defaultConfig
#if MIN_VERSION_fsnotify(0, 4, 0)
#else
                        { FN.confDebounce = FN.Debounce (fromIntegral (1 :: Int)) }
                        -- 把时间相近(1秒内)的事件合并
                        -- 很多对文件操作的工具保存时都可能由多个文件系统操作完成
                        -- 比如 vim
#endif

        handle_evt get_atk' fp' evt = do
            case evt of
                FN.Removed {} -> do
                    -- do not handle remove event
                    -- when modify file with editor like vim, file maybe removed then save again
                    -- To remove a menu, modify the menu to be empty list
                    return ()

                _ -> do
                    let evt_fp = normalise $ FN.eventPath evt
                    if evt_fp `elem` fp'
                        then do
                            --- $logDebugS wxppLogSource $ fromString (show evt)
                            -- 用 vim 在线修改文件时，总是收到一个 Removed 的事件
                            -- 干脆不理会 event 的类型，直接检查文件是否存在
                            exists <- liftIO $ threadDelay (500 * 1000) >> doesFileExist evt_fp
                            if not exists
                                then do
                                    $logWarnS wxppLogSource $
                                        "menu config file has been removed or inaccessible: "
                                            <> (fromString $ FN.eventPath evt)
                                    -- 如果打算停用菜单，可直接将配置文件清空
                                else do
                                    load get_atk' fp'
                        else do
                            --- $logDebugS wxppLogSource $ fromString $ "skipping notification for path: " <> evt_fp
                            return ()

        load :: IO (Maybe AccessToken) -> [FilePath] -> m ()
        load get_atk' normalized_fp = do
            m_atk <- liftIO get_atk'
            case m_atk of
                Nothing             -> do
                    $logErrorS  wxppLogSource $ "Failed to create menu: no access token available."

                Just access_token   ->  do
                    let app_id = accessTokenApp access_token
                        data_dirs = fmap takeDirectory normalized_fp
                    err_or <- wxppCreateMenuWithYaml access_token (LNE.fromList data_dirs) fname
                    case err_or of
                        Left err    -> $logErrorS  wxppLogSource $
                                                "Failed to create menu for app: "
                                                <> unWxppAppID app_id
                                                <> ", error was: " <> fromString err
                        Right _     -> $logInfoS wxppLogSource $
                                            "menu reloaded for app: " <> unWxppAppID app_id
