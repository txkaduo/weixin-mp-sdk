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
-- import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC8
import Blaze.ByteString.Builder             (toLazyByteString)

import Yesod.Helpers.Types

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Yesod.Site


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


-- | 调用中心 wxpp 服务器的接口：根据 union_id 找所有匹配的 open_id
-- 注意：如果微信平台让两个不相干的 app 的用户产生相同的 union id
--       这个函数的返回值就会包含多余的数据
--       因为它的逻辑就是所有已知的 union id 等于指定值的用户的
--       (open_id, app_id) tupple
-- XXX: 要访问这个 route，目前要知道一个 app id
--      但实际上其内部逻辑根本不需要任何 app id
wxppCenterLookupOpenID ::
    (MonadIO m, MonadThrow m) =>
    UrlText
    -> WxppUnionID
    -> m [(WxppOpenID, WxppAppID)]
wxppCenterLookupOpenID base_url union_id = do
    let url = mkWxppSubNoAppUrl base_url $ LookupOpenIDByUnionIDR union_id
    let opts = defaults
    (liftIO $ getWith opts url) >>= asWxppWsResponseNormal'


--------------------------------------------------------------------------------

mkWxppSubNoAppUrl ::
    UrlText -> Route WxppSubNoApp -> String
mkWxppSubNoAppUrl base_url route =
    LBC8.unpack $ toLazyByteString $
        joinPath partial_foundation (unUrlText base_url) ps qs
    where
        partial_foundation = (LiteApp (error "unLiteApp forced"))
        (ps, qs) = renderRoute route

partialWxppSubNoApp :: WxppSubNoApp
partialWxppSubNoApp = WxppSubNoApp
                        (error "wxppSubNoAppUnionIdByOpenId forced")
                        (error "wxppSubNoAppRunLoggingT forced")
                        (error "wxppSubNoAppCheckWaiReq forced")

partialWxppAppConfig :: WxppAppID -> WxppAppConfig
partialWxppAppConfig app_id = WxppAppConfig app_id
                            (error "wxppConfigAppSecret forced")
                            (error "wxppConfigAppToken forced")
                            (error "wxppConfigAppAesKey forced")
                            (error "wxppConfigAppBackupAesKeys forced")
                            (error "wxppAppConfigDataDir forced")

partialWxppSub :: WxppAppID -> WxppSub
partialWxppSub app_id = WxppSub (partialWxppAppConfig app_id)
                            (error "wxppSubAccessTokens forced")
                            (error "wxppSubGetUnionID forced")
                            (error "wxppSubRunDBActionRunner forced")
                            (error "wxppSubSendOutMsgs forced")
                            (error "wxppSubMsgHandler forced")
                            (error "wxppSubRunLoggingT forced")
                            (error "wxppSubOptions forced")
