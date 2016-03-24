{-# LANGUAGE ScopedTypeVariables #-}
module  WeiXin.PublicPlatform.Utils where

import ClassyPrelude
import qualified Data.QRCode                as QR   -- haskell-qrencode
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString            as B
import qualified Codec.Picture              as P -- from `JuicyPixel' package
import qualified Codec.Picture.Saving       as P -- from `JuicyPixel' package
import qualified Data.Text                  as T
import qualified Data.Serialize             as S
import qualified Data.StateVar              as SV
import qualified Data.Aeson                 as A
import qualified Data.Yaml                  as Y
import qualified Data.HashMap.Strict        as HM

import Data.Array                           (bounds,(!), array)
import Data.List                            ((!!))
import Data.Time                            (NominalDiffTime)
import Data.Time.Clock.POSIX                ( posixSecondsToUTCTime
                                            , utcTimeToPOSIXSeconds)
import System.FilePath                      (hasExtension)
import System.Directory                     (doesFileExist)
import Network.Mime                         (MimeType, defaultMimeType)
import Network.Wreq                         (Response, responseBody, responseHeader)
import Control.Lens                         ((^.), (^?))
import Control.Monad.Trans.Maybe
import Data.Aeson.Types                     (Parser)
import Data.Aeson
import Data.Yaml                            (ParseException, prettyPrintParseException)
import qualified Data.Yaml                  as Yaml
import Control.Monad.Catch                  ( Handler(..) )
import Data.List.NonEmpty                   as LNE hiding (length, (!!))

import Crypto.Hash.TX.Utils                 (MD5Hash(..), md5HashBS)

epochIntToUtcTime :: Int64 -> UTCTime
epochIntToUtcTime = posixSecondsToUTCTime . (realToFrac :: Int64 -> NominalDiffTime)

utcTimeToEpochInt :: UTCTime -> Int64
utcTimeToEpochInt = round . utcTimeToPOSIXSeconds


data YamlFileParseException = YamlFileParseException
                                    String          -- file path
                                    ParseException  -- the real exception
                                deriving (Typeable)

instance Show YamlFileParseException where
    show (YamlFileParseException fp exc) = fp ++ ": " ++ prettyPrintParseException exc

instance Exception YamlFileParseException

-- | 在实现允许在解释一个 YAML 时，引用另一个 YAML这样的功能时
-- 在第一次解释时，结果不再是一个简单的值，而是一个 IO 函数。
-- 以下类型就是表达这种延期加载的函数
-- ReaderT 的 r 参数是相对路径相对的目录
type DelayedYamlLoader a = ReaderT
                                (NonEmpty FilePath)
                                    -- 运行时要知道一个或更多的消息文件的基础目录
                                IO
                                (Either YamlFileParseException a)


-- | Cache read result from file system
type CachedYamlInfo = HashMap
                        FilePath  -- full file path
                        (MD5Hash, Value)
                                  -- file content md5 and its Yaml parsing result

type CachedYamlLoader s a = ReaderT s IO a

-- | intend to obsolete DelayedYamlLoader
type DelayedCachedYamlLoader s a = NonEmpty FilePath -> CachedYamlLoader s a

type CachedYamlInfoState s = ( SV.HasUpdate s CachedYamlInfo CachedYamlInfo
                             , SV.HasGetter s CachedYamlInfo
                             )

setExtIfNotExist :: String -> FilePath -> FilePath
setExtIfNotExist def_ext fp =
    if hasExtension fp
       then fp
       else fp <.> def_ext

-- | 从一个字典对象中解释出某种值
-- 这个字典里约定两个字段
-- 一个是直接可以解释出所需的值的字段，即值直接内嵌在当前字典中
-- 另一个是指示出一个文件路径的字段，此路径用于真正延迟加载的情况
-- 如果不指定第一个字段，那么就认为当前的字典可以解释出所需的值
parseDelayedYamlLoader :: FromJSON a =>
  Maybe Text
  -> Text
  -> Object -> Parser (DelayedYamlLoader a)
parseDelayedYamlLoader m_direct_field indirect_field obj =
    case m_direct_field of
        Nothing ->
            -- 在没有指定直接字段时，优先试一次间接字段
            parse_indirect <|> (return . Right <$> parseJSON (toJSON obj))
        Just direct_field ->
            (return . Right <$> obj .: direct_field) <|> parse_indirect
    where
        parse_indirect = mkDelayedYamlLoader . setExtIfNotExist "yml" . T.unpack <$> obj .: indirect_field


mkDelayedYamlLoader :: forall a. FromJSON a => FilePath -> DelayedYamlLoader a
mkDelayedYamlLoader fp = do
    base_dir_list <- ask
    liftIO $ do
        -- 运行时，如果全部都有 IOError，抛出第一个非 DoesNotExistError
        let try_dir :: FilePath -> IO (Either (Either IOError YamlFileParseException) a)
            try_dir bp = do
                let full_path = bp </> fp
                ioerr_or_v <- tryIOError $ do
                                -- we need to catch IOError, so don't use decodeFileEither
                                -- decodeFileEither full_path
                                (liftM Yaml.decodeEither' $ B.readFile full_path)

                return $ case ioerr_or_v of
                            Left ioe          -> Left $ Left ioe
                            Right (Left perr) -> Left $ Right $ YamlFileParseException full_path perr
                            Right (Right x)   -> Right x

        first_try <- try_dir $ LNE.head base_dir_list
        case first_try of
            Right v         -> return $ Right v
            Left first_err  -> do
                let is_does_not_exist_err (Left ioe) = isDoesNotExistError ioe
                    is_does_not_exist_err (Right _)  = False
                let go []       last_err    = either throwM (return . Left) last_err
                    go (x:xs)   last_err    = do
                        try_dir x
                            >>= either
                                    (\e -> go xs $ if is_does_not_exist_err last_err then e else last_err)
                                    (return . Right)

                go (LNE.tail base_dir_list) first_err

-- | May throw IOError
runDelayedYamlLoaderL_IOE :: (MonadIO m, FromJSON a) =>
    NonEmpty FilePath    -- ^ 消息文件存放目录
    -> DelayedYamlLoader a
    -> m (Either YamlFileParseException a)
runDelayedYamlLoaderL_IOE base_dirs f = liftIO $ runReaderT f base_dirs

runDelayedYamlLoaderL :: (MonadIO m, FromJSON a) =>
    NonEmpty FilePath    -- ^ 消息文件存放目录
    -> DelayedYamlLoader a
    -> m (Either String a)
runDelayedYamlLoaderL base_dir_list get_ext = liftIO $ do
    err_or_x <- tryIOError $ runReaderT get_ext base_dir_list
    case err_or_x of
        Left err    -> return $ Left $ "failed to load from file: " ++ show err
        Right x     -> return $ parseMsgErrorToString x

runDelayedYamlLoader :: (MonadIO m, FromJSON a) =>
    FilePath    -- ^ 消息文件存放目录
    -> DelayedYamlLoader a
    -> m (Either String a)
runDelayedYamlLoader base_dir = runDelayedYamlLoaderL (base_dir :| [])

-- | 这是会抛异常的版本
runDelayedYamlLoaderExcL :: (MonadIO m, FromJSON a, MonadThrow m) =>
    NonEmpty FilePath        -- ^ 消息文件存放目录
    -> DelayedYamlLoader a
    -> m a
runDelayedYamlLoaderExcL base_dir_list get_ext = liftIO $
    runReaderT get_ext base_dir_list
        >>= either throwM return

runDelayedYamlLoaderExc :: (MonadIO m, FromJSON a, MonadThrow m) =>
    FilePath    -- ^ 消息文件存放目录
    -> DelayedYamlLoader a
    -> m a
runDelayedYamlLoaderExc base_dir = runDelayedYamlLoaderExcL (base_dir :| [])


findFirstMatchedFile :: (Functor t, Foldable t)
                     => t FilePath  -- ^ base dirs to try
                     -> FilePath    -- ^ sub-path
                     -> IO (Maybe FilePath)
                      -- ^ get the first fullpath that exists
findFirstMatchedFile base_dir_list fp = do
  runMaybeT $ asum $ fmap (MaybeT . try_dir) $ base_dir_list
  where
    try_dir bp = do
        let full_path = bp </> fp

        exists <- doesFileExist full_path
        return $ if exists
                   then Just full_path
                   else Nothing


mkDelayedCachedYamlLoader :: ( FromJSON a, CachedYamlInfoState s)
                          => FilePath
                          -> DelayedCachedYamlLoader s a
mkDelayedCachedYamlLoader fp base_dir_list = do
  full_path <- liftIO (findFirstMatchedFile base_dir_list fp)
                >>= maybe
                    (throwM $ mkIOError doesNotExistErrorType "mkDelayedCachedYamlLoader" Nothing (Just fp))
                    return

  bs <- liftIO $ B.readFile full_path
  let md5sum = md5HashBS bs

  st <- ask
  cache_map <- SV.get st
  jv <- case HM.lookup full_path cache_map of
          Just (md5sum2, jv) | md5sum == md5sum2 -> do
            return jv

          _ -> do
            new_jv <- either (throwM . YamlFileParseException full_path) return $ Y.decodeEither' bs
            st SV.$~! (HM.insert full_path (md5sum, new_jv))
            return new_jv

  case A.fromJSON jv of
    A.Error err -> throwM $ YamlFileParseException full_path (Y.AesonException err)
    A.Success x -> return x


-- | May throw IOError
runDelayedCachedYamlLoaderL_IOE :: ( MonadIO m, FromJSON a, CachedYamlInfoState s)
                                => s
                                -> NonEmpty FilePath    -- ^ 消息文件存放目录
                                -> DelayedCachedYamlLoader s a
                                -> m (Either YamlFileParseException a)
runDelayedCachedYamlLoaderL_IOE st base_dirs f = liftIO $ try $ runReaderT (f base_dirs) st

-- | Don't throw IOError
runDelayedCachedYamlLoaderL :: ( MonadIO m, FromJSON a, CachedYamlInfoState s)
                            => s
                            -> NonEmpty FilePath    -- ^ 消息文件存放目录
                            -> DelayedCachedYamlLoader s a
                            -> m (Either String a)
runDelayedCachedYamlLoaderL st base_dirs f = liftIO $ do
  err_or_x <- tryIOError $ try $ runReaderT (f base_dirs) st
  case err_or_x of
      Left err    -> return $ Left $ "failed to load from file: " ++ show err
      Right x     -> return $ parseMsgErrorToString x


parseMsgErrorToString :: Either YamlFileParseException a -> Either String a
parseMsgErrorToString (Left err)    = Left $ "failed to parse from file: " ++ show err
parseMsgErrorToString (Right x)     = Right x

unifyExcHandler :: (Show e, Monad m) => Handler m (Either e a) -> Handler m (Either String a)
unifyExcHandler = fmap $ either (Left . show) Right


-- | 生成 JuicyPixels Image 对象
-- copied and modified from: https://gist.github.com/minoki/7d6a610fe03fd84122d5
encodeStringQRCodeImage :: MonadIO m =>
                           Int
                           -> String
                           -> m (P.Image P.Pixel8)
encodeStringQRCodeImage pixelPerCell input = do
    qrcode <- liftIO $ QR.encodeString input Nothing QR.QR_ECLEVEL_M QR.QR_MODE_EIGHT True
    return $ renderQRCodeToImage pixelPerCell qrcode


-- XXX: Data.QRCode do not export QRCode, we cannot write type signature here.
-- renderQRCodeToImage :: (Bounded px, P.Pixel px) => Int -> QR.QRCode -> P.Image px
renderQRCodeToImage pixelPerCell qrcode = image
  where
        matrix = QR.toMatrix qrcode
        dim1 = length matrix
        dim2 = fromMaybe 0 $ fmap length $ listToMaybe matrix
        to_on_off x = if x /= 0 then minBound else maxBound
        arr = array ((0, 0), (dim1-1, dim2-1)) $ do
                    y <- [0..dim1-1]
                    x <- [0..dim2-1]
                    return $ ((y, x), to_on_off $ (matrix !! y) !! x)
        ((y0,x0),(y1,x1)) = bounds arr
        pixelAt x y = let x' = x `div` pixelPerCell
                          y' = y `div` pixelPerCell
                          i = y'+y0
                          j = x'+x0
                      in arr !(i,j)
        image = P.generateImage pixelAt ((x1-x0+1)*pixelPerCell) ((y1-y0+1)*pixelPerCell)


encodeStringQRCodeJpeg :: MonadIO m =>
                        Int
                        -> String
                        -> m LB.ByteString
encodeStringQRCodeJpeg pixelPerCell input =
    liftM (P.imageToJpg 100 . P.ImageY8) $ encodeStringQRCodeImage pixelPerCell input


-- | 经常需要保存微信的文件内容及mime到本地
-- 这个类型及下面的工具函数为此而设
type FileContentAndMime = (LB.ByteString, MimeType)

saveWreqResponseContentAndMime :: (MonadIO m)
                                => Handle
                                -> Response LB.ByteString
                                -> m FileContentAndMime
saveWreqResponseContentAndMime h r = liftIO $ do
    LB.hPut h $ S.encodeLazy dat
    return dat
    where
        dat  = (body, mime)
        body = r ^. responseBody
        mime = fromMaybe defaultMimeType $ r ^? responseHeader "Content-Type"

loadWreqResponseContentAndMime :: (MonadIO m)
                                => Handle -- ^ 因为使用了 hGetContents ，Handle 会关闭
                                -> m FileContentAndMime
loadWreqResponseContentAndMime h = liftIO $ do
    lbs <- LB.hGetContents h
    either (const $ throwM $ userError "cannot decode file as FileContentAndMime") return $ S.decodeLazy lbs
