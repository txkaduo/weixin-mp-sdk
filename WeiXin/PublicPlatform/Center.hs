module WeiXin.PublicPlatform.Center
    ( module WeiXin.PublicPlatform.Center
    , module WeiXin.PublicPlatform.Types
    , module WeiXin.PublicPlatform.WS
    ) where

import ClassyPrelude
import Network.Wreq
-- import Control.Lens hiding ((.=))
-- import Control.Monad.Logger
import Yesod.Core

import qualified Data.Text                  as T

import Yesod.Helpers.Types

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS


-- | 调用中心 wxpp 服务器的接口： 取 AccessToken
wxppCenterGetAccessToken ::
    ( MonadIO m, MonadThrow m) =>
    UrlText
    -> WxppAppID
    -> m AccessToken
wxppCenterGetAccessToken base_url app_id = do
    let url = T.unpack $ T.intercalate "/"
                            [ unUrlText base_url, toPathPiece app_id, "x/atk" ]
        opts = defaults
    (liftIO $ getWith opts url) >>= asWxppWsResponseNormal'


-- | 调用中心 wxpp 服务器的接口： 取 UnionID
wxppCenterLookupUnionID ::
    ( MonadIO m, MonadThrow m) =>
    UrlText
    -> WxppAppID
    -> WxppOpenID
    -> m (Maybe WxppUnionID)
wxppCenterLookupUnionID base_url app_id open_id = do
    let url = T.unpack $ T.intercalate "/"
                            [ unUrlText base_url, toPathPiece app_id
                            , "x/union_id"
                            , toPathPiece open_id
                            ]
        opts = defaults
    (liftIO $ getWith opts url) >>= asWxppWsResponseNormal'


-- | 调用中心 wxpp 服务器的接口： 取 QueryUserInfo
wxppCenterQueryUserInfo ::
    (MonadIO m, MonadThrow m) =>
    UrlText
    -> WxppAppID
    -> WxppOpenID
    -> m EndUserQueryResult
wxppCenterQueryUserInfo base_url app_id open_id = do
    let url = T.unpack $ T.intercalate "/"
                            [ unUrlText base_url, toPathPiece app_id
                            , "x/user/info"
                            , toPathPiece open_id
                            ]
        opts = defaults
    (liftIO $ getWith opts url) >>= asWxppWsResponseNormal'
