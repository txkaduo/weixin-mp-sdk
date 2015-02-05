module WeiXin.PublicPlatform.InMsgHandler where

import ClassyPrelude
import Control.Monad.Logger
import Control.Monad.Trans.Except
import qualified Data.Text                  as T
import Data.Aeson
import Data.Aeson.Types                     (Parser)
import Data.Yaml                            (decodeFileEither, parseEither, ParseException(..))

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Media
import WeiXin.PublicPlatform.Acid

-- | 响应收到的服务器信息
-- Left 用于表达错误
-- Right Nothing 代表无需回复一个新信息
type WxppInMsgHandler m = WxppInMsgEntity
                            -> m (Either String (Maybe WxppOutMsg))

class FromJsonHandler h where
    -- | 假定每个算法的配置段都有一个 name 的字段
    -- 根据这个方法选择出一个指定算法类型，
    -- 然后从 json 数据中反解出相应的值
    isNameOfInMsgHandler :: Monad n => n h -> Text -> Bool

    parseInMsgHandler :: Monad n => n h -> Object -> Parser h

-- | something that can be used as WxppInMsgHandler
class IsWxppInMsgHandler m h where
    handleInMsg :: h
                -> AcidState WxppAcidState
                -> m (Maybe AccessToken)
                -> WxppInMsgHandler m


data SomeWxppInMsgHandler m =
        forall h. (IsWxppInMsgHandler m h, FromJsonHandler h) => SomeWxppInMsgHandler h

instance IsWxppInMsgHandler m (SomeWxppInMsgHandler m) where
    handleInMsg (SomeWxppInMsgHandler h) = handleInMsg h

-- | 用于在配置文件中，读取出一系列响应算法
parseWxppInMsgHandlers ::
    [SomeWxppInMsgHandler m]
        -- ^ value inside SomeWxppInMsgHandler is not used
        -- use: SomeWxppInMsgHandler undefined is ok
    -> Value
    -> Parser [SomeWxppInMsgHandler m]
parseWxppInMsgHandlers known_hs = withArray "[SomeWxppInMsgHandler]" $
        mapM (parseWxppInMsgHandler known_hs) . toList

parseWxppInMsgHandler ::
    [SomeWxppInMsgHandler m]
        -- ^ value inside SomeWxppInMsgHandler is not used
        -- use: SomeWxppInMsgHandler undefined is ok
        -- see: allBasicWxppInMsgHandlersWHNF
    -> Value
    -> Parser (SomeWxppInMsgHandler m)
parseWxppInMsgHandler known_hs =
    withObject "SomeWxppInMsgHandler" $ \obj -> do
        name <- obj .: "name"
        SomeWxppInMsgHandler h <- maybe
                (fail $ "unknown handler name: " <> T.unpack name)
                return
                $ flip find known_hs
                $ \(SomeWxppInMsgHandler h) -> isNameOfInMsgHandler (Just h) name
        fmap SomeWxppInMsgHandler $ parseInMsgHandler (Just h) obj


-- | 这里构造的列表只用于 parseWxppInMsgHandlers
allBasicWxppInMsgHandlersWHNF ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m ) =>
    [SomeWxppInMsgHandler m]
allBasicWxppInMsgHandlersWHNF =
    [ SomeWxppInMsgHandler (error "handler forced" :: WelcomeSubscribe)
    ]


readWxppInMsgHandlers ::
    [SomeWxppInMsgHandler m]
    -> String
    -> IO (Either ParseException [SomeWxppInMsgHandler m])
readWxppInMsgHandlers tmps fp = runExceptT $ do
    (ExceptT $ decodeFileEither fp)
        >>= either (throwE . AesonException) return
                . parseEither (parseWxppInMsgHandlers tmps)


-- | 使用列表里的所有算法，逐一调用一次以处理收到的信息
-- 返回第一个返回 Right Just 的结果
-- 如果都没有返回 Right Just 则：
-- 如果有一个handler返回 Right Nothing，这个函数也返回 Right Nothing
-- （代表成功但无回复）
-- 如果没有一个handler返回 Right Nothing，则：
--   若有返回 Left 的，则选择第一个 Left 返回（代表失败）
--   若连 Left 也没有（那只可能出现在handler数量本身就是零的情况）
--      则理解为成功
tryEveryInMsgHandler :: MonadLogger m =>
    [WxppInMsgHandler m] -> WxppInMsgHandler m
tryEveryInMsgHandler handlers ime = do
    (errs, res_lst) <- liftM partitionEithers $
                            mapM (\h -> h ime) handlers
    forM_ errs $ \err -> do
        $(logWarnS) wxppLogSource $ T.pack $
            "Error when handling incoming message, "
            <> "MsgId=" <> (show $ wxppInMessageID ime)
            <> ": " <> err

    case catMaybes res_lst of
        []      -> do
                    -- 没有一个 handler 有回复
                    -- 如果 res_lst 本身也是空，说明全部都失败了，
                    -- 除非errs也为空
                    return $
                        if null res_lst
                            then maybe (Right Nothing) Left $
                                    listToMaybe errs
                            else Right Nothing
        (x:xs)  -> do
                    when (not $ null xs) $ do
                        -- 有多个 Just 结果，但服务器只能让我们回复一个信息
                        -- 因此后面的信息会目前会丢失
                        -- TODO：以后可以异步调用“客服接口”主动发消息给用户
                        --       根据文档，这只能在 48 小时完成
                        $(logWarnS) wxppLogSource $ T.pack $
                            "more than one reply messages from handlers,"
                            <> " incoming MsgId=" <> (show $ wxppInMessageID ime)
                    return $ Right $ Just x


tryEveryInMsgHandler' :: MonadLogger m =>
    AcidState WxppAcidState
    -> m (Maybe AccessToken)
    -> [SomeWxppInMsgHandler m]
    -> WxppInMsgHandler m
tryEveryInMsgHandler' acid get_atk known_hs = do
    tryEveryInMsgHandler $ flip map known_hs $ \h -> handleInMsg h acid get_atk

-- | 用户订阅公众号时发送欢迎信息
data WelcomeSubscribe = WelcomeSubscribe WxppOutMsgL
                        deriving (Show, Typeable)

instance FromJsonHandler WelcomeSubscribe where
    isNameOfInMsgHandler _ x = x == "welcome-subscribe"

    parseInMsgHandler _ obj = do
        WelcomeSubscribe <$> obj .: "msg"


instance ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    IsWxppInMsgHandler m WelcomeSubscribe
    where
    handleInMsg (WelcomeSubscribe outmsg) acid get_atk ime = do
        is_subs <- case wxppInMessage ime of
                    WxppInMsgEvent WxppEvtSubscribe             -> return True
                    WxppInMsgEvent (WxppEvtSubscribeAtScene {}) -> return True
                    _   -> return False
        if is_subs
            then do
                err_or <- tryWxppWsResult $ do
                    m_atk <- get_atk
                    case m_atk of
                        Nothing -> return $ Left "no access token available"
                        Just atk -> do
                            outmsg' <- fromWxppOutMsgL acid atk outmsg
                            return $ Right $ Just outmsg'
                case err_or of
                    Left err -> do
                        return $ Left $
                            "Got exception when getting access token or uploading media: "
                                <> show err
                    Right x -> return x
            else return $ Right Nothing
