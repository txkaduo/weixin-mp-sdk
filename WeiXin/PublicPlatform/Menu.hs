{-#LANGUAGE CPP #-}
{-#LANGUAGE FlexibleContexts #-}
module WeiXin.PublicPlatform.Menu where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Data.Aeson
import Control.Monad.Trans.Except
import Data.Yaml                            (decodeFileEither)
import Filesystem.Path.CurrentOS            (encodeString, toText)
import qualified System.FSNotify            as FN
import Control.Monad.Trans.Control
import System.Directory                     (doesFileExist)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Error
import WeiXin.PublicPlatform.WS


-- | result in query menu
data MenuDef = MenuDef [MenuItem]

instance FromJSON MenuDef where
    parseJSON = withObject "MenuDef" $ \obj -> do
                    obj .: "menu"
                        >>= withObject "Menu" (\o -> fmap MenuDef $ o .: "button")


-- | 调用服务器接口，创建菜单
wxppCreateMenu ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> [MenuItem] -> m ()
wxppCreateMenu (AccessToken access_token) menus = do
    let url = wxppRemoteApiBaseUrl <> "/menu/create"
        opts = defaults & param "access_token" .~ [ access_token ]
    err_resp@(WxppAppError err _msg) <-
                (liftIO $ postWith opts url $ toJSON $ object [ "button" .= menus ])
                    >>= liftM (view responseBody) . asJSON
    when ( err /= WxppErrorX (Right WxppNoError) ) $ do
        throwM err_resp


-- | 调用服务器接口，查询菜单
wxppQueryMenu ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> m [MenuItem]
wxppQueryMenu (AccessToken access_token) = do
    let url = wxppRemoteApiBaseUrl <> "/menu/get"
        opts = defaults & param "access_token" .~ [ access_token ]
    MenuDef items <- (liftIO $ getWith opts url)
                        >>= asWxppWsResponseNormal'
    return items


-- | 调用服务器接口，删除菜单
wxppDeleteMenu ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> m ()
wxppDeleteMenu (AccessToken access_token) = do
    let url = wxppRemoteApiBaseUrl <> "/menu/delete"
        opts = defaults & param "access_token" .~ [ access_token ]
    err_resp@(WxppAppError err _msg) <-
                (liftIO $ getWith opts url)
                    >>= liftM (view responseBody) . asJSON
    when ( err /= WxppErrorX (Right WxppNoError) ) $ do
        throwM err_resp


-- | 根据指定 YAML 文件配置调用远程接口，修改菜单
wxppCreateWithYaml :: (MonadIO m, MonadLogger m, MonadCatch m) =>
    AccessToken -> FilePath -> m (Either String ())
wxppCreateWithYaml access_token fp = runExceptT $ do
    err_or_menu <- liftIO $ decodeFileEither $ encodeString fp
    case err_or_menu of
        Left err    -> do
            $(logErrorS) wxppLogSource $
                "Failed to parse menu yml: " <> fromString (show err)
            throwE $ "Failed to parse yml: " <> show err
        Right menu  -> do
            err_or <- tryWxppWsResult $
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
wxppWatchMenuYaml :: (MonadIO m, MonadLogger m, MonadCatch m, MonadBaseControl IO m) =>
    m (Maybe AccessToken)
    -> IO ()        -- ^ bloack until exit
    -> FilePath
    -> m ()
wxppWatchMenuYaml get_atk block_until_exit fp = do
    -- 主动加载一次菜单配置
    now <- liftIO getCurrentTime
    handle_evt $ FN.Added fp now

    bracket (liftIO $ FN.startManagerConf watch_cfg) (liftIO . FN.stopManager) $ \mgr -> do
        stop <- (liftBaseOpDiscard $
                    FN.watchDir
                        mgr
                        dir
                        ((== fn) . filename . FN.eventPath)
                ) handle_evt
        liftIO $ block_until_exit >> stop
    where
        dir = directory fp
        fn = filename fp

        watch_cfg = FN.defaultConfig
                        { FN.confDebounce = FN.Debounce (fromIntegral (1 :: Int)) }
                        -- 把时间相近(1秒内)的事件合并
                        -- 很多对文件操作的工具保存时都可能由多个文件系统操作完成
                        -- 比如 vim

        handle_evt evt = do
            -- 用 vim 在线修改文件时，总是收到一个 Removed 的事件
            -- 干脆不理会 event 的类型，直接检查文件是否存在
            exists <- liftIO $ doesFileExist $ encodeString fp
            if not exists
                then do
                    $logWarnS wxppLogSource $ "menu config file has been removed or inaccessible: "
                                                <> (either id id $ toText (FN.eventPath evt))
                    -- 如果打算停用菜单，可直接将配置文件清空
                else do
                    m_atk <- get_atk
                    case m_atk of
                        Nothing             -> do
                            $logErrorS  wxppLogSource $ "Failed to create menu: no access token available."
                        Just access_token   ->  do
                            err_or <- wxppCreateWithYaml access_token (FN.eventPath evt)
                            case err_or of
                                Left err    -> $logErrorS  wxppLogSource $ fromString $
                                                        "Failed to create menu: " <> err
                                Right _     -> $logInfoS wxppLogSource $ "menu reloaded."

