module WeiXin.PublicPlatform.WS where

import ClassyPrelude hiding (catch)
import Network.Wreq
import Control.Lens hiding ((.=))
import qualified Data.ByteString.Lazy       as LB
import Control.Monad.Catch                  ( Handler(..), catches )
--import Control.Monad.Trans.Control          (MonadBaseControl)

import Network.HTTP.Client                  (HttpException(..))
import Data.Aeson                           ( withObject, (.:)
                                            , FromJSON(..)
                                            , ToJSON(..)
                                            , (.:), (.:?), (.!=), (.=)
                                            , object
                                            )

import WeiXin.PublicPlatform.Error


-- | 平台服务器返回错误的通用格式
data WxppAppError = WxppAppError
                            WxppErrorX
                            Text
                        deriving (Show, Typeable)

instance Exception WxppAppError

instance FromJSON WxppAppError where
    parseJSON = withObject "WxppAppError" $ \obj -> do
                    ec <- obj .: "errcode"
                    msg <- obj .:? "errmsg" .!= ""
                    return $ WxppAppError
                                (wxppFromErrorCode ec)
                                msg

instance ToJSON WxppAppError where
    toJSON (WxppAppError ex t) = object [ "errcode" .= wxppToErrorCodeX ex
                                        , "errmsg"  .= t
                                        ]

-- | 从文档上看，接口的返回值内容，正常与错误两种情况都用一种方式返回
-- 所以，实现时，要用试的方式去解释服务器返回的内容
newtype WxppWsResp a = WxppWsResp {
                            unWxppWsResp :: Either WxppAppError a
                            }

instance FromJSON a => FromJSON (WxppWsResp a) where
    parseJSON v = fmap WxppWsResp $
                    fmap Left (parseJSON v) <|> fmap Right (parseJSON v)

instance ToJSON a => ToJSON (WxppWsResp a) where
    toJSON (WxppWsResp (Left x))    = toJSON x
    toJSON (WxppWsResp (Right x))   = toJSON x


-- | “预期”之内的错误类型
data WxppWsCallError =  WxppWsErrorHttp HttpException
                            -- ^ 所有网络通讯层的错误
                        | WxppWsErrorJson JSONError
                            -- ^ 返回格式按JSON解释出现的错误
                        | WxppWsErrorApp WxppAppError
                            -- ^ 业务层报告的错误
                        deriving  (Show, Typeable)

instance Exception WxppWsCallError


wxppWsExcHandlers :: Monad m => [Handler m (Either WxppWsCallError a)]
wxppWsExcHandlers = [ Handler h1, Handler h2, Handler h3 ]
    where
        h1 = return . Left . WxppWsErrorHttp
        h2 = return . Left . WxppWsErrorJson
        h3 = return . Left . WxppWsErrorApp

tryWxppWsResult :: MonadCatch m =>
    m a -> m (Either WxppWsCallError a)
tryWxppWsResult f = liftM Right f `catches` wxppWsExcHandlers


asWxppWsResponseNormal :: (MonadThrow m, FromJSON a) =>
    Response LB.ByteString -> m (WxppWsResp a)
asWxppWsResponseNormal r = do
    -- workaround:
    -- 平台返回JSON报文时，并不会把 Content-Type 设为 JSON，而是 text/plain
    -- 所以，先把 response 的 content-type 改了，试一次当前错误报文解释一次
    liftM (view responseBody) $ asJSON (alterContentTypeToJson r)


alterContentTypeToJson :: Response body -> Response body
alterContentTypeToJson r =
    if r ^. responseHeader "Content-Type" == "text/plain"
            then r & responseHeader "Content-Type" .~ "application/json"
            else r

-- | 解释远程调用的结果，返回正常的值，异常情况会抛出
-- 抛出的异常主要类型就是 WxppWsCallError 列出的情况
asWxppWsResponseNormal' :: (MonadThrow m, FromJSON a) =>
    Response LB.ByteString -> m a
asWxppWsResponseNormal' =
    asWxppWsResponseNormal
        >=> either throwM return . unWxppWsResp


wxppRemoteApiBaseUrl :: IsString a => a
wxppRemoteApiBaseUrl = "https://api.weixin.qq.com/cgi-bin"

wxppRemoteFileApiBaseUrl :: IsString a => a
wxppRemoteFileApiBaseUrl = "https://file.api.weixin.qq.com/cgi-bin"
