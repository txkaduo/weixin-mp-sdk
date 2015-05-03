module WeiXin.PublicPlatform.Menu where

import ClassyPrelude
import Network.Wreq
import Control.Lens hiding ((.=))
import Control.Monad.Logger
import Data.Aeson
import Control.Monad.Trans.Except
import Data.Yaml                            (decodeFileEither)

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
    AccessToken -> String -> m (Either String ())
wxppCreateWithYaml access_token fp = runExceptT $ do
    err_or_menu <- liftIO $ decodeFileEither fp
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
