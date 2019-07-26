{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module WeiXin.PublicPlatform.WS where

-- {{{1 imports
import ClassyPrelude
import Network.Wreq
import Control.Monad.Except
import Control.Monad.Logger
import Control.Lens hiding ((.=))
import qualified Data.ByteString.Lazy       as LB
import qualified Control.Exception.Safe     as ExcSafe


import Network.HTTP.Client                  (HttpException(..))
import qualified Network.Wreq.Session       as WS
import Data.Aeson                           ( withObject, (.:)
                                            , FromJSON(..)
                                            , ToJSON(..)
                                            , (.:), (.:?), (.!=), (.=)
                                            , object
                                            )

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP

import WeiXin.PublicPlatform.Error
import WeiXin.PublicPlatform.Types

import Yesod.Compat
-- }}}1


class HasWreqSession a where
    getWreqSession :: a -> WS.Session

instance HasWreqSession WS.Session where
    getWreqSession = id

instance HasWreqSession a => HasWreqSession (a, b) where
    getWreqSession = getWreqSession .fst


class HasWxppUrlConfig a where
    getWxppUrlConfig :: a -> WxppUrlConfig

instance HasWxppUrlConfig WxppUrlConfig where
    getWxppUrlConfig = id

instance HasWxppUrlConfig a => HasWxppUrlConfig (a, b) where
    getWxppUrlConfig = getWxppUrlConfig . fst


data WxppApiEnv = WxppApiEnv WS.Session WxppUrlConfig

instance HasWreqSession WxppApiEnv where
  getWreqSession (WxppApiEnv sess _) = sess

instance HasWxppUrlConfig WxppApiEnv where
  getWxppUrlConfig (WxppApiEnv _ c) = c


class HasWxppApiEnv a where
  getWxppApiEnv :: a -> WxppApiEnv

instance HasWxppApiEnv WxppApiEnv where
  getWxppApiEnv = id


type WxppApiMonad r m = ( MonadIO m, MonadLogger m
                        , MonadReader r m
                        , HasWreqSession r
                        , HasWxppUrlConfig r
                        )


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

-- | 从文档上看，接口的返回值内容，正常与错误两种情况都用一个 JSON 值
-- 没有其它可靠的区分方式。
-- 所以，实现时，要用试的方式去解释服务器返回的内容
-- 注意：它的 FromJSON 实现时，先尝试当成错误报文解释，然后再尝试当成正常报文解释
--       这意味着假定了正常报文不会被当成错误报文解释。
--       这个假定绝大多数时候都成立，但有个别例外。
--       例外的例子就是：群发接口中上传图文消息素材等多个接口返回报文格式与错误报文区别太小。
--       对于这种例子，应使用 WxppWsResp2
--       UPDATE: 现在会解释成错误报文时会先检查错误代码是不是零，若是，则继续放弃作为错误报文解释，
--               改作正常报文解释
newtype WxppWsResp a = WxppWsResp {
                            unWxppWsResp :: Either WxppAppError a
                            }

-- {{{1 instances
instance FromJSON a => FromJSON (WxppWsResp a) where
    parseJSON v = fmap WxppWsResp $ fmap Left try_ex_no_err <|> fmap Right (parseJSON v)
      where
        try_ex_no_err = do
          ex@(WxppAppError e _) <- parseJSON v
          guard $ e /= WxppErrorX (Right WxppNoError)
          return ex

instance ToJSON a => ToJSON (WxppWsResp a) where
    toJSON (WxppWsResp (Left x))    = toJSON x
    toJSON (WxppWsResp (Right x))   = toJSON x
-- }}}1


newtype WxppWsResp2 a = WxppWsResp2 {
                            unWxppWsResp2 :: Either WxppAppError (Text, a)
                            }

instance FromJSON a => FromJSON (WxppWsResp2 a) where
    parseJSON = withObject "WxppWsResp2" $ \o -> do
                    let v = toJSON o
                    err@(WxppAppError ex msg) <- parseJSON v
                    liftM WxppWsResp2 $ do
                        if ( ex == WxppErrorX (Right WxppNoError) )
                            then do
                                x <- parseJSON v
                                return $ Right (msg, x)
                            else do
                                return $ Left err

-- | “预期”之内的错误类型
data WxppWsCallError =  WxppWsErrorHttp HttpException
                            -- ^ 所有网络通讯层的错误
                        | WxppWsErrorJson JSONError
                            -- ^ 返回格式按JSON解释出现的错误
                        | WxppWsErrorApp WxppAppError
                            -- ^ 业务层报告的错误
                        deriving  (Show, Typeable)

instance Exception WxppWsCallError

wxppCallWxError :: WxppWsCallError -> Maybe WxppErrorX
wxppCallWxError (WxppWsErrorApp (WxppAppError err _msg)) = Just err
wxppCallWxError _ = Nothing


wxppWsExcHandlers :: Monad m => [ExcSafe.Handler m (Either WxppWsCallError a)]
wxppWsExcHandlers = [ ExcSafe.Handler h1, ExcSafe.Handler h2, ExcSafe.Handler h3 ]
    where
        h1 = return . Left . WxppWsErrorHttp
        h2 = return . Left . WxppWsErrorJson
        h3 = return . Left . WxppWsErrorApp

tryWxppWsResult :: ExcSafe.MonadCatch m => m a -> m (Either WxppWsCallError a)
tryWxppWsResult f = liftM Right f `ExcSafe.catches` wxppWsExcHandlers


tryWxppWsResultE :: (ExcSafe.MonadCatch m, IsString e)
                 => String -> ExceptT e m b -> ExceptT e m b
tryWxppWsResultE op_name f =
-- Because there is no instance MonadUnliftIO (ExceptT e m)
#if MIN_VERSION_classy_prelude(1, 5, 0)
  lift (tryWxppWsResult $ runExceptT f)
    >>= either (\e -> throwError $ fromString $ "Got Exception when " <> op_name <> ": " <> show e) return
    >>= either throwError return
#else
    tryWxppWsResult f
        >>= either (\e -> throwError $ fromString $ "Got Exception when " <> op_name <> ": " <> show e) return
#endif

asWxppWsResponseJson :: (MonadIO m, MonadLogger m, FromJSON a) => Response LB.ByteString -> m a
asWxppWsResponseJson r = do
  -- workaround:
  -- 平台返回JSON报文时，并不会把 Content-Type 设为 JSON，而是 text/plain
  -- 所以，先把 response 的 content-type 改了，试一次当前错误报文解释一次
  let r' = alterContentTypeToJson r
  let body = view responseBody r'

  case A.eitherDecode' body of
    Left err -> do
      $logErrorS wxppLogSource $ "Response body cannot be converted to json: " <> fromString err
        <> ", content was:\n" <> toStrict (decodeUtf8 body)
      liftIO $ throwIO $ JSONError err

    Right jv -> do
      case A.fromJSON jv of
        A.Error err -> do
          $logErrorS wxppLogSource $ "Response body cannot be converted to expected json structure: " <> fromString err
            <> ", content was:\n" <> toStrict (decodeUtf8 $ AP.encodePretty jv)
          liftIO $ throwIO $ JSONError err
        A.Success x -> return x


asWxppWsResponseNormal :: (MonadIO m, MonadLogger m, FromJSON a) => Response LB.ByteString -> m (WxppWsResp a)
asWxppWsResponseNormal = asWxppWsResponseJson

asWxppWsResponseNormal2 :: (MonadIO m, MonadLogger m, FromJSON a) => Response LB.ByteString -> m (WxppWsResp2 a)
asWxppWsResponseNormal2 = asWxppWsResponseJson


asWxppWsResponseVoid :: (MonadIO m) => Response LB.ByteString -> m ()
asWxppWsResponseVoid r = do
    err_resp@(WxppAppError err _msg) <-
        liftIO $ liftM (view responseBody) $ asJSON (alterContentTypeToJson r)
    when ( err /= WxppErrorX (Right WxppNoError) ) $ do
        liftIO $ throwIO err_resp

alterContentTypeToJson :: Response body -> Response body
alterContentTypeToJson r =
    if r ^. responseHeader "Content-Type" == "text/plain"
            then r & responseHeader "Content-Type" .~ "application/json"
            else r

-- | 解释远程调用的结果，返回正常的值，异常情况会抛出
-- 抛出的异常主要类型就是 WxppWsCallError 列出的情况
asWxppWsResponseNormal' :: (MonadIO m, MonadLogger m, FromJSON a)
                        => Response LB.ByteString
                        -> m a
asWxppWsResponseNormal' =
    asWxppWsResponseNormal >=> either (liftIO . throwIO) return . unWxppWsResp


-- | Like asWxppWsResponseNormal', but log responseBody when got exception
asWxppWsResponseNormal'L :: ( FromJSON a, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 5, 0)
                            -- , MonadIO m
#else
                            , MonadIO m
#endif
                            , MaskExceptionMonad m
                            )
                         => Response LB.ByteString
                         -> m a
asWxppWsResponseNormal'L resp = do
  let report = $logErrorS wxppLogSource $
                  "Invalid diagram:\n" <> toStrict (decodeUtf8 $ resp ^. responseBody) <> "\n"

  asWxppWsResponseNormal' resp `onException` report


asWxppWsResponseNormal2' :: (MonadIO m, MonadLogger m, FromJSON a)
                         => Response LB.ByteString
                         -> m (Text, a)
asWxppWsResponseNormal2' =
    asWxppWsResponseNormal2
        >=> either (liftIO . throwIO) return . unWxppWsResp2


-- vim: set foldmethod=marker:
