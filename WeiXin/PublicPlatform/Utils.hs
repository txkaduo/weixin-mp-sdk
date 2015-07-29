module  WeiXin.PublicPlatform.Utils where

import ClassyPrelude hiding (FilePath, (<.>), (</>))
import qualified Data.QRCode                as QR   -- haskell-qrencode
import qualified Data.ByteString.Lazy       as LB
import qualified Codec.Picture              as P -- from `JuicyPixel' package
import qualified Codec.Picture.Saving       as P -- from `JuicyPixel' package

import Data.Array                           (bounds,(!), array)
import Data.List                            ((!!))
import Data.Time                            (NominalDiffTime)
import Data.Time.Clock.POSIX                ( posixSecondsToUTCTime
                                            , utcTimeToPOSIXSeconds)
import Filesystem.Path.CurrentOS            (encodeString, fromText, extension, FilePath, (<.>), (</>))
import Data.Aeson.Types                     (Parser)
import Data.Aeson
import Data.Yaml                            (ParseException, decodeFileEither)
import Control.Monad.Catch                  ( Handler(..) )

epochIntToUtcTime :: Int64 -> UTCTime
epochIntToUtcTime = posixSecondsToUTCTime . (realToFrac :: Int64 -> NominalDiffTime)

utcTimeToEpochInt :: UTCTime -> Int64
utcTimeToEpochInt = round . utcTimeToPOSIXSeconds

-- | 在实现允许在解释一个 YAML 时，引用另一个 YAML这样的功能时
-- 在第一次解释时，结果不再是一个简单的值，而是一个 IO 函数。
-- 以下类型就是表达这种延期加载的函数
-- ReaderT 的 r 参数是相对路径相对的目录
type DelayedYamlLoader a = ReaderT FilePath IO (Either ParseException a)


setExtIfNotExist :: Text -> FilePath -> FilePath
setExtIfNotExist def_ext fp =
    maybe (fp <.> def_ext) (const fp) $ extension fp

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
        parse_indirect = mkDelayedYamlLoader . setExtIfNotExist "yml" . fromText <$> obj .: indirect_field

mkDelayedYamlLoader :: FromJSON a => FilePath -> DelayedYamlLoader a
mkDelayedYamlLoader fp = do
    base_dir <- ask
    liftIO $ decodeFileEither (encodeString $ base_dir </> fp)

runDelayedYamlLoader :: (MonadIO m, FromJSON a) =>
    FilePath    -- ^ 消息文件存放目录
    -> DelayedYamlLoader a
    -> m (Either String a)
runDelayedYamlLoader base_dir get_ext = liftIO $ do
    err_or_x <- tryIOError $ runReaderT get_ext base_dir
    case err_or_x of
        Left err    -> return $ Left $ "failed to load from file: " ++ show err
        Right x     -> return $ parseMsgErrorToString x

-- | 这是会抛异常的版本
runDelayedYamlLoaderExc :: (MonadIO m, FromJSON a, MonadThrow m) =>
    FilePath    -- ^ 消息文件存放目录
    -> DelayedYamlLoader a
    -> m a
runDelayedYamlLoaderExc base_dir get_ext = liftIO $
    runReaderT get_ext base_dir
        >>= either throwM return

parseMsgErrorToString :: Either ParseException a -> Either String a
parseMsgErrorToString (Left err)   = Left $ "failed to parse from file: " ++ show err
parseMsgErrorToString (Right x)    = Right x

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
    let matrix = QR.toMatrix qrcode
        dim1 = length matrix
        dim2 = fromMaybe 0 $ fmap length $ listToMaybe matrix
        to_on_off x = if x /= 0 then minBound else maxBound
        arr = array ((0, 0), (dim1-1, dim2-1)) $ do
                    y <- [0..dim1-1]
                    x <- [0..dim2-1]
                    return $ ((y, x), to_on_off $ (matrix !! y) !! x)
    let ((y0,x0),(y1,x1)) = bounds arr
        pixelAt x y = let x' = x `div` pixelPerCell
                          y' = y `div` pixelPerCell
                          i = y'+y0
                          j = x'+x0
                      in arr !(i,j)
        image = P.generateImage pixelAt ((x1-x0+1)*pixelPerCell) ((y1-y0+1)*pixelPerCell)
    return image

encodeStringQRCodeJpeg :: MonadIO m =>
                        Int
                        -> String
                        -> m LB.ByteString
encodeStringQRCodeJpeg pixelPerCell input =
    liftM (P.imageToJpg 100 . P.ImageY8) $ encodeStringQRCodeImage pixelPerCell input
