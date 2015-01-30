module WeiXin.PublicPlatform.Menu where

import ClassyPrelude
import Network.Wreq
import Control.Lens
import Control.Monad.Logger
import Data.Aeson                           (toJSON)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Error
import WeiXin.PublicPlatform.WS


-- | 调用服务器接口，创建菜单
wxppCreateMenu ::
    ( MonadIO m, MonadLogger m, MonadThrow m) =>
    AccessToken -> [MenuItem] -> m ()
wxppCreateMenu (AccessToken access_token) menus = do
    let url = wxppRemoteApiBaseUrl <> "/menu/create"
        opts = defaults & param "access_token" .~ [ access_token ]
    err_resp@(WxppAppError err _msg) <-
                (liftIO $ postWith opts url $ toJSON menus)
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
    (liftIO $ getWith opts url)
                >>= asWxppWsResponseNormal'


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
