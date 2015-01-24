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
data WxppWsError = WxppWsError
                            WxppErrorX
                            Text
                        deriving (Show, Typeable)

instance Exception WxppWsError

instance FromJSON WxppWsError where
    parseJSON = withObject "WxppWsError" $ \obj -> do
                    ec <- obj .: "errcode"
                    msg <- obj .:? "errmsg" .!= ""
                    return $ WxppWsError
                                (wxppFromErrorCode ec)
                                msg


-- | 从文档上看，接口的返回值内容，正常与错误两种情况都用一种方式返回
-- 所以，实现时，要用试的方式去解释服务器返回的内容
newtype WxppWsResp a = WxppWsResp {
                            unWxppWsResp :: Either WxppWsError a
                            }

instance FromJSON a => FromJSON (WxppWsResp a) where
    parseJSON v = fmap WxppWsResp $
                    fmap Left (parseJSON v) <|> fmap Right (parseJSON v)


-- | 服务器可能在 HTTP 层上出错，也可能在 API 报文内报错
-- 这个类型方便分层处理错误
type WxppWsResult a = Either HttpException (Either WxppWsError a)

-- | 这个类型方便统一处理两种错误
type WxppWsResultP a = Either (Either HttpException WxppWsError) a

packWsError :: WxppWsResult a -> WxppWsResultP a
packWsError (Left x)          = Left $ Left x
packWsError (Right (Left x))  = Left $ Right x
packWsError (Right (Right x)) = Right x

unpackWsError :: WxppWsResultP a -> WxppWsResult a
unpackWsError (Left (Left x))     = Left x
unpackWsError (Left (Right x))    = Right (Left x)
unpackWsError (Right x)           = Right (Right x)

tryWxppWsResult :: MonadCatch m => m a -> m (WxppWsResultP a)
tryWxppWsResult f = do
    liftM Right f `catch` h1 `catch` h2
    where
        h1 = return . Left . Left
        h2 = return . Left . Right


asWxppWsResponseNormal :: (MonadThrow m, FromJSON a) =>
    Response LB.ByteString
    -> m (Either WxppWsError (Response a))
asWxppWsResponseNormal rb = do
    r <- liftM (over responseBody unWxppWsResp) $ asJSON rb
    return $ case r ^. responseBody of
        Left err -> Left err
        Right nb -> Right $ r & responseBody .~ nb


wxppRemoteApiBaseUrl :: IsString a => a
wxppRemoteApiBaseUrl = "https://api.weixin.qq.com/cgi-bin"
