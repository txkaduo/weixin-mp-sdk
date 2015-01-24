module WeiXin.PublicPlatform.WS where

import ClassyPrelude hiding (catch)
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy       as LB
import Control.Monad.Catch                  ( catch )
--import Control.Monad.Trans.Control          (MonadBaseControl)

import Network.HTTP.Client                  (HttpException(..))
import Data.Aeson                           ( withObject, (.:)
                                            , FromJSON(..)
                                            , (.:), (.:?), (.!=)
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


-- | 从文档上看，接口的返回值内容，正常与错误两种情况都用一种方式返回
-- 所以，实现时，要用试的方式去解释服务器返回的内容
newtype WxppWsResp a = WxppWsResp {
                            unWxppWsResp :: Either WxppAppError a
                            }

instance FromJSON a => FromJSON (WxppWsResp a) where
    parseJSON v = fmap WxppWsResp $
                    fmap Left (parseJSON v) <|> fmap Right (parseJSON v)


-- | “预期”之内的错误类型
data WxppWsCallError =  WxppWsErrorHttp HttpException
                            -- ^ 所有网络通讯层的错误
                        | WxppWsErrorJson JSONError
                            -- ^ 返回格式按JSON解释出现的错误
                        | WxppWsErrorApp WxppAppError
                            -- ^ 业务层报告的错误
                        deriving  (Show, Typeable)

instance Exception WxppWsCallError


tryWxppWsResult :: MonadCatch m =>
    m a -> m (Either WxppWsCallError a)
tryWxppWsResult f = do
    liftM Right f `catch` h1 `catch` h2
    where
        h1 = return . Left . WxppWsErrorHttp
        h2 = return . Left . WxppWsErrorJson


asWxppWsResponseNormal :: (MonadThrow m, FromJSON a) =>
    Response LB.ByteString -> m (WxppWsResp a)
asWxppWsResponseNormal = liftM (view responseBody) . asJSON

asWxppWsResponseNormal' :: (MonadThrow m, FromJSON a) =>
    Response LB.ByteString -> m a
asWxppWsResponseNormal' =
    asWxppWsResponseNormal
        >=> either (throwM . WxppWsErrorApp) return . unWxppWsResp


wxppRemoteApiBaseUrl :: IsString a => a
wxppRemoteApiBaseUrl = "https://api.weixin.qq.com/cgi-bin"
