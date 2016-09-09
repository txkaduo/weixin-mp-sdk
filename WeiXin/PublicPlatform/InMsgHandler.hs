{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module WeiXin.PublicPlatform.InMsgHandler where

import ClassyPrelude hiding (catch)
import Network.Wreq hiding (Proxy)
import Control.Lens hiding ((<.>), op)
import Data.Proxy
import Control.DeepSeq                      (($!!))
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Monad.Catch                  ( catchAll )
import Control.Monad.Trans.Maybe
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Map.Strict            as Map
import Data.List.NonEmpty                   as LNE (NonEmpty, head)
import Data.Aeson
import Data.Aeson.Types                     (Parser)
import Data.Yaml                            (decodeFileEither, parseEither, ParseException(..))
import Data.Time                            (NominalDiffTime)

import Text.Regex.TDFA                      (blankExecOpt, blankCompOpt, Regex)
import Text.Regex.TDFA.TDFA                 ( examineDFA)
import Text.Regex.TDFA.String               (compile, execute)
import Control.Monad.Catch                  (catch)
import System.Directory                     (canonicalizePath)
import System.FilePath                      (splitDirectories)

import Yesod.Helpers.Aeson                  (parseArray)
import Yesod.Helpers.Parsec                 (CharParser)
import Text.Parsec                          (parse)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Media
import WeiXin.PublicPlatform.EndUser
import WeiXin.PublicPlatform.Utils



-- | 对收到的消息处理的函数
type WxppInMsgProcessor m a =
        ProcAppIdInfo
        -> LB.ByteString
            -- ^ raw data of message (unparsed)
        -> WxppInMsgEntity
            -- ^ this is nothing only if caller cannot parse the message
        -> m (Either String a)
            -- ^ Left 用于表达错误

-- | 对于收到的消息有的回应，可能有多个。
-- 分为两类：主要的，和备选的
-- 备选的回应仅当“主要的”回应不存在是使用
-- 主要的回应如果有多个，会尽量全部发给用户。但由于微信接口的限制，只有
-- 第一个回应可以以回复的方式发给用户，其它“主要”回应要通过“客服”接口发给
-- 用户，而“客服”接口需要一定条件才能开通。
-- Bool 表明这个响应是否是“主要”的
-- Nothing 代表无需回复一个新信息
type WxppInMsgHandlerResult = [(Bool, Maybe WxppOutMsg)]

type WxppInMsgHandler m = WxppInMsgProcessor m WxppInMsgHandlerResult

wxppInMsgHandleSingleTextOut :: Bool -> Text -> WxppInMsgHandlerResult
wxppInMsgHandleSingleTextOut is_prim t = return (is_prim, Just (WxppOutMsgText t))

hasPrimaryOutMsg :: WxppInMsgHandlerResult -> Bool
hasPrimaryOutMsg = isJust . find fst

hasPrimaryOutMsgE :: Either a WxppInMsgHandlerResult -> Bool
hasPrimaryOutMsgE (Left _)  = False
hasPrimaryOutMsgE (Right x) = hasPrimaryOutMsg x


-- | 可以从配置文件中读取出来的某种处理值
class JsonConfigable h where
    -- | 从配置文件中读取的数据未必能提供构造完整的值的所有信息
    -- 这个类型指示出无法在配置文件中提供的信息
    -- 这部分信息只能由调用者自己提供
    -- 通常这会是一个函数
    type JsonConfigableUnconfigData h

    -- | 假定每个算法的配置段都有一个 name 的字段
    -- 根据这个方法选择出一个指定算法类型，
    -- 然后从 json 数据中反解出相应的值
    isNameOfInMsgHandler :: Proxy h -> Text -> Bool

    parseWithExtraData :: Proxy h -> JsonConfigableUnconfigData h -> Object -> Parser h

isNameOfInMsgHandler' :: forall h. JsonConfigable h => h -> Text -> Bool
isNameOfInMsgHandler' _ = isNameOfInMsgHandler (Proxy :: Proxy h)

-- | 预处理收到的消息的结果
type family WxppInMsgProcessResult h :: *


-- | 对收到的消息作出某种处理
-- 实例分为两大类：
-- Predictor 其处理结果是个 Bool 值
-- Handler 其处理结果是个 WxppInMsgHandlerResult
class IsWxppInMsgProcessor m h where
    processInMsg :: (WxppCacheTokenReader c, WxppCacheTemp c) =>
        h
        -> c
        -> WxppInMsgProcessor m (WxppInMsgProcessResult h)

data SomeWxppInMsgProcessor r m =
        forall h. (IsWxppInMsgProcessor m h, WxppInMsgProcessResult h ~ r) => SomeWxppInMsgProcessor h


type instance WxppInMsgProcessResult (SomeWxppInMsgProcessor r m) = r

-- | 所有 Handler 可放在这个类型内
type SomeWxppInMsgHandler m = SomeWxppInMsgProcessor WxppInMsgHandlerResult m

-- | 所有 Predictor 可放在这个类型内
type SomeWxppInMsgPredictor m = SomeWxppInMsgProcessor Bool m


-- | something that can be used as WxppInMsgHandler
type IsWxppInMsgHandler m h =
        ( IsWxppInMsgProcessor m h
        , WxppInMsgProcessResult h ~ WxppInMsgHandlerResult
        )

instance IsWxppInMsgProcessor m (SomeWxppInMsgProcessor r m) where
    processInMsg (SomeWxppInMsgProcessor h) = processInMsg h


data WxppInMsgProcessorPrototype r m =
        forall h. (IsWxppInMsgProcessor m h, JsonConfigable h, WxppInMsgProcessResult h ~ r) =>
                WxppInMsgProcessorPrototype (Proxy h) (JsonConfigableUnconfigData h)

type WxppInMsgHandlerPrototype m = WxppInMsgProcessorPrototype WxppInMsgHandlerResult m

type WxppInMsgPredictorPrototype m = WxppInMsgProcessorPrototype Bool m

type IsWxppInMsgHandlerRouter m p =
            ( IsWxppInMsgProcessor m p, JsonConfigable p
            , WxppInMsgProcessResult p ~ Maybe (SomeWxppInMsgHandler m)
            )

data SomeWxppInMsgHandlerRouter m =
        forall p. ( IsWxppInMsgHandlerRouter m p ) =>
            SomeWxppInMsgHandlerRouter p



class Monad m => IsWxppInMsgProcMiddleware m a where
    preProcInMsg :: (WxppCacheTokenReader c, WxppCacheTemp c)
                 => a
                 -> c
                 -> ProcAppIdInfo
                 -> LB.ByteString
                 -- ^ raw data of message (unparsed)
                 -> WxppInMsgEntity
                 -- ^ this is nothing only if caller cannot parse the message
                 -> m (Maybe (LB.ByteString, WxppInMsgEntity))
    preProcInMsg _ _ _ bs ime = return (Just (bs, ime))

    postProcInMsg :: a
                  -> ProcAppIdInfo
                  -> LB.ByteString
                  -- ^ raw data of message (unparsed)
                  -> WxppInMsgEntity
                  -- ^ this is nothing only if caller cannot parse the message
                  -> WxppInMsgHandlerResult
                  -> m WxppInMsgHandlerResult
    postProcInMsg _ _ _ _ x = return x

    -- | callback when something goes wrong
    onProcInMsgError :: a
                     -> ProcAppIdInfo
                     -> LB.ByteString
                     -- ^ raw data of message (unparsed)
                     -> WxppInMsgEntity
                     -- ^ this is nothing only if caller cannot parse the message
                     -> String
                     -> m ()
    onProcInMsgError _ _ _ _ _ = return ()


data SomeWxppInMsgProcMiddleware m =
        forall a. (IsWxppInMsgProcMiddleware m a, JsonConfigable a) => SomeWxppInMsgProcMiddleware a

data WxppInMsgProcMiddlewarePrototype m =
        forall a. (IsWxppInMsgProcMiddleware m a, JsonConfigable a) =>
                WxppInMsgProcMiddlewarePrototype (Proxy a) (JsonConfigableUnconfigData a)


-- | 一种可调用任意指定函数的middleware
-- 但因为太任意，不方便给定一个固定的名字，
-- 所以目前不能从配置文件中选择性地引入(即readWxppInMsgProcMiddlewares)
-- 而是要写死在 Haskell 代码里
data WrapPreProcMsg m = WrapPreProcMsg
                          (forall c.(WxppCacheTokenReader c, WxppCacheTemp c)
                            => c
                            -> ProcAppIdInfo
                            -> LB.ByteString
                            -> WxppInMsgEntity
                            -> m (Maybe (LB.ByteString, WxppInMsgEntity))
                          )

instance Monad m => IsWxppInMsgProcMiddleware m (WrapPreProcMsg m) where
  preProcInMsg (WrapPreProcMsg f) = f


-- | 用于在配置文件中，读取出一系列响应算法
parseWxppInMsgProcessors ::
    [WxppInMsgProcessorPrototype r m]
    -> Value
    -> Parser [SomeWxppInMsgProcessor r m]
parseWxppInMsgProcessors known_hs = withArray "[SomeWxppInMsgProcessor]" $
        mapM (withObject "SomeWxppInMsgProcessor" $ parseWxppInMsgProcessor known_hs) . toList

parseWxppInMsgProcessor ::
    [WxppInMsgProcessorPrototype r m]
    -> Object
    -> Parser (SomeWxppInMsgProcessor r m)
parseWxppInMsgProcessor known_hs obj = do
        name <- obj .: "name"
        WxppInMsgProcessorPrototype ph ext <- maybe
                (fail $ "unknown processor name: " <> T.unpack name)
                return
                $ flip find known_hs
                $ \(WxppInMsgProcessorPrototype ph _) -> isNameOfInMsgHandler ph name
        fmap SomeWxppInMsgProcessor $ parseWithExtraData ph ext obj


readWxppInMsgHandlers ::
    [WxppInMsgHandlerPrototype m]
    -> NonEmpty FilePath
    -> FilePath
    -> IO (Either YamlFileParseException [SomeWxppInMsgHandler m])
readWxppInMsgHandlers tmps data_dirs fp = runExceptT $ do
    (ExceptT $ runDelayedYamlLoaderL_IOE data_dirs $ mkDelayedYamlLoader fp)
        >>= either (throwE . YamlFileParseException fp . AesonException) return
                . parseEither (parseWxppInMsgProcessors tmps)


readWxppInMsgHandlersCached :: (CachedYamlInfoState s)
                            => s
                            -> [WxppInMsgHandlerPrototype m]
                            -> NonEmpty FilePath
                            -> FilePath
                            -> IO (Either YamlFileParseException [SomeWxppInMsgHandler m])
readWxppInMsgHandlersCached st tmps data_dirs fp = runExceptT $ do
    ExceptT (runDelayedCachedYamlLoaderL_IOE st data_dirs $ mkDelayedCachedYamlLoader fp)
        >>= either (throwE . YamlFileParseException fp . AesonException) return
                . parseEither (parseWxppInMsgProcessors tmps)


-- | 用于在配置文件中，读取出一系列响应算法
parseWxppInMsgProcMiddlewares ::
    [WxppInMsgProcMiddlewarePrototype m]
    -> Value
    -> Parser [SomeWxppInMsgProcMiddleware m]
parseWxppInMsgProcMiddlewares known_hs = withArray "[SomeWxppInMsgProcMiddleware]" $
    mapM (withObject "SomeWxppInMsgProcMiddleware" $ parseWxppInMsgProcMiddleware known_hs) . toList

parseWxppInMsgProcMiddleware ::
    [WxppInMsgProcMiddlewarePrototype m]
    -> Object
    -> Parser (SomeWxppInMsgProcMiddleware m)
parseWxppInMsgProcMiddleware known_hs obj = do
        name <- obj .: "name"
        WxppInMsgProcMiddlewarePrototype ph ext <- maybe
                (fail $ "unknown middleware name: " <> T.unpack name)
                return
                $ flip find known_hs
                $ \(WxppInMsgProcMiddlewarePrototype ph _) -> isNameOfInMsgHandler ph name
        fmap SomeWxppInMsgProcMiddleware $ parseWithExtraData ph ext obj

readWxppInMsgProcMiddlewares ::
    [WxppInMsgProcMiddlewarePrototype m]
    -> String
    -> IO (Either ParseException [SomeWxppInMsgProcMiddleware m])
readWxppInMsgProcMiddlewares tmps fp = runExceptT $ do
    (ExceptT $ decodeFileEither fp)
        >>= either (throwE . AesonException) return
                . parseEither (parseWxppInMsgProcMiddlewares tmps)


-- | 使用列表里的所有算法，逐一调用一次以处理收到的信息
tryEveryInMsgHandler :: MonadLogger m =>
    [WxppInMsgHandler m] -> WxppInMsgHandler m
tryEveryInMsgHandler handlers app_info bs ime = do
    (errs, res_lst) <- liftM partitionEithers $
                          mapM (\h -> h app_info bs ime) handlers
    forM_ errs $ \err -> do
        $(logWarnS) wxppLogSource $ T.pack $
            "Error when handling incoming message, "
            <> "MsgId=" <> (show $ wxppInMessageID ime)
            <> ": " <> err

    return $ Right $ join res_lst

tryEveryInMsgHandler' :: (MonadLogger m, WxppCacheTokenReader c, WxppCacheTemp c) =>
    c
    -> [SomeWxppInMsgHandler m]
    -> WxppInMsgHandler m
tryEveryInMsgHandler' cache known_hs = do
    tryEveryInMsgHandler $ flip map known_hs $ \h -> processInMsg h cache


-- | 逐一试各个函数，直至有一个提供“主”回应
tryInMsgHandlerUntilFirstPrimary :: MonadLogger m =>
    [WxppInMsgHandler m] -> WxppInMsgHandler m
tryInMsgHandlerUntilFirstPrimary handlers app_info bs ime = do
    (errs, res_lst) <- liftM (partitionEithers . reverse) $ go [] handlers
    forM_ errs $ \err -> do
        $(logWarnS) wxppLogSource $ T.pack $
            "Error when handling incoming message, "
            <> "MsgId=" <> (show $ fmap unWxppInMsgID $ wxppInMessageID ime)
            <> ": " <> err

    return $ Right $ join res_lst
    where
        go rs [] = return rs
        go rs (x:xs) = do
            r <- x app_info bs ime
            let rs' = r:rs
            if isJust $ find fst $ join $ rights rs'
                then return rs'
                else go rs' xs

tryInMsgHandlerUntilFirstPrimary' :: (MonadLogger m, WxppCacheTokenReader c, WxppCacheTemp c) =>
    c
    -> [SomeWxppInMsgHandler m]
    -> WxppInMsgHandler m
tryInMsgHandlerUntilFirstPrimary' cache known_hs =
    tryInMsgHandlerUntilFirstPrimary $ flip map known_hs $ \h -> processInMsg h cache


preProcessInMsgByMiddlewares :: (Monad m, WxppCacheTokenReader c, WxppCacheTemp c)
                             => [SomeWxppInMsgProcMiddleware m]
                             -> c
                             -> ProcAppIdInfo
                             -> LB.ByteString
                             -- ^ raw data of message (unparsed)
                             -> WxppInMsgEntity
                             -- ^ this is nothing only if caller cannot parse the message
                             -> m (Maybe (LB.ByteString, WxppInMsgEntity))
preProcessInMsgByMiddlewares [] _cache _app_info bs ime = return $ Just (bs, ime)
preProcessInMsgByMiddlewares (SomeWxppInMsgProcMiddleware x:xs) cache app_info bs ime =
    runMaybeT $ do
      (bs', ime') <- MaybeT $ preProcInMsg x cache app_info bs ime
      MaybeT $ preProcessInMsgByMiddlewares xs cache app_info bs' ime'

postProcessInMsgByMiddlewares :: (Monad m)
                              => [SomeWxppInMsgProcMiddleware m]
                              -> ProcAppIdInfo
                              -> LB.ByteString
                                  -- ^ raw data of message (unparsed)
                              -> WxppInMsgEntity
                                  -- ^ this is nothing only if caller cannot parse the message
                              -> WxppInMsgHandlerResult
                              -> m WxppInMsgHandlerResult
postProcessInMsgByMiddlewares [] _app_info _bs _ime res = return res
postProcessInMsgByMiddlewares (SomeWxppInMsgProcMiddleware x:xs) app_info bs ime res =
  postProcInMsg x app_info bs ime res >>= postProcessInMsgByMiddlewares xs app_info bs ime


onErrorProcessInMsgByMiddlewares :: (Monad m)
                                 => [SomeWxppInMsgProcMiddleware m]
                                 -> ProcAppIdInfo
                                 -> LB.ByteString
                                     -- ^ raw data of message (unparsed)
                                 -> WxppInMsgEntity
                                     -- ^ this is nothing only if caller cannot parse the message
                                 -> String
                                 -> m ()
onErrorProcessInMsgByMiddlewares xs app_info bs ime err =
  forM_ xs $ \(SomeWxppInMsgProcMiddleware x) -> onProcInMsgError x app_info bs ime err


tryYamlExcE :: MonadCatch m => ExceptT String m b -> ExceptT String m b
tryYamlExcE f =
    f `catch` (\e -> throwE $ show (e :: ParseException))

parseWxppOutMsgLoader :: Object -> Parser WxppOutMsgLoader
parseWxppOutMsgLoader obj = do
    -- parseDelayedYamlLoader (Just "msg") "file"
    -- 逐一尝试以下字段
    -- msg: 内嵌表示的完整消息
    -- article-file: 外部文件定义的单图文消息
    -- file: 外部文件定义的完整消息
    parse_direct
        <|> (fmap (fmap $ \x -> WxppOutMsgNewsL [ return (Right x) ] ) <$> parse_indirect1)
        <|> parse_indirect2
    where
        parse_indirect1 = mkDelayedYamlLoader . setExtIfNotExist "yml" . T.unpack <$> obj .: "article-file"
        parse_indirect2 = mkDelayedYamlLoader . setExtIfNotExist "yml" . T.unpack <$> obj .: "file"
        parse_direct    = return . Right <$> obj .: "msg"


-- | Handler: 处理收到的信息的算法例子：用户订阅公众号时发送欢迎信息
data WelcomeSubscribe = WelcomeSubscribe
                            (NonEmpty FilePath) -- 所有消息文件存放的目录
                            Bool                -- if primary
                            WxppOutMsgLoader    -- 打算回复用户的消息
                        deriving (Typeable)

instance JsonConfigable WelcomeSubscribe where
    type JsonConfigableUnconfigData WelcomeSubscribe = NonEmpty FilePath

    isNameOfInMsgHandler _ x = x == "welcome-subscribe"

    parseWithExtraData _ msg_dirs obj = do
        WelcomeSubscribe msg_dirs
                <$> (obj .:? "primary" .!= False)
                <*> parseWxppOutMsgLoader obj


type instance WxppInMsgProcessResult WelcomeSubscribe = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m WelcomeSubscribe
    where

    processInMsg (WelcomeSubscribe msg_dirs primary get_outmsg) cache app_info _bs ime = runExceptT $ do
        is_subs <- case wxppInMessage ime of
                    WxppInMsgEvent WxppEvtSubscribe              -> return True
                    WxppInMsgEvent (WxppEvtSubscribeAtScene {})  -> return True
                    _                                                   -> return False
        if is_subs
            then do
                let get_atk = (tryWxppWsResultE "getting access token" $ liftIO $
                                    wxppCacheGetAccessToken cache app_id)
                                >>= maybe (throwE $ "no access token available") (return . fst)
                outmsg <- ExceptT $ runDelayedYamlLoaderL msg_dirs get_outmsg
                liftM (return . (primary,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                                tryYamlExcE $ fromWxppOutMsgL msg_dirs cache get_atk outmsg
            else return []
        where
          app_id = procAppIdInfoReceiverId app_info


-- | Handler: 处理点击菜单项目的事件通知，加载 Key 参数中指定的文件所记录的消息
-- 要求 Key 参数的格式为： send-msg:<path to yaml>
data WxppInMsgMenuItemClickSendMsg = WxppInMsgMenuItemClickSendMsg
                                        (NonEmpty FilePath)

instance JsonConfigable WxppInMsgMenuItemClickSendMsg where
    type JsonConfigableUnconfigData WxppInMsgMenuItemClickSendMsg = NonEmpty FilePath

    isNameOfInMsgHandler _ x = x == "menu-click-send-msg"

    parseWithExtraData _ msg_dirs _obj =
        return $ WxppInMsgMenuItemClickSendMsg msg_dirs


type instance WxppInMsgProcessResult WxppInMsgMenuItemClickSendMsg = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgMenuItemClickSendMsg
    where

    processInMsg (WxppInMsgMenuItemClickSendMsg msg_dirs) cache app_info _bs ime = runExceptT $ do
        let m_fp = do
                case wxppInMessage ime of
                    WxppInMsgEvent (WxppEvtClickItem evt_key) -> do
                            T.stripPrefix "send-msg:" evt_key

                    _ -> Nothing

        case m_fp of
            Nothing -> return []
            Just fp' -> do
                let get_atk = (tryWxppWsResultE "getting access token" $ liftIO $
                                    wxppCacheGetAccessToken cache app_id)
                                >>= maybe (throwE $ "no access token available") (return . fst)
                let fp = setExtIfNotExist "yml" $ T.unpack fp'
                outmsg <- ExceptT $ runDelayedYamlLoaderL msg_dirs $ mkDelayedYamlLoader fp
                liftM (return . (True,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                                tryYamlExcE $ fromWxppOutMsgL msg_dirs cache get_atk outmsg

        where
          app_id = procAppIdInfoReceiverId app_info


-- | used in WxppInMsgMenuItemClickGeneral
class MenuItemEventKey a where
    -- | used to generate unique name for isNameOfInMsgHandler
    menuItemEventKeyIdent   :: Proxy a -> Text

    -- | parse the event key string
    menuItemEventKeyParser  :: CharParser a


class MenuItemEventKey a => MenuItemEventKeyHandle m a where
    type MenuItemEventKeyHandleEnv m a
    -- | handle the event
    handleMenuItemEventKey  :: (WxppCacheTokenReader c, WxppCacheTemp c) =>
                            a
                            -> MenuItemEventKeyHandleEnv m a
                            -> c
                            -> WxppInMsgHandler m

-- | Handler: 处理点击菜单项目的事件通知，比较通用的版本
-- 真正的逻辑都在 MenuItemEventKey class 里
data WxppInMsgMenuItemClickGeneral (m :: * -> *) a = WxppInMsgMenuItemClickGeneral
                                                        (MenuItemEventKeyHandleEnv m a)

instance MenuItemEventKey a => JsonConfigable (WxppInMsgMenuItemClickGeneral m a) where
    type JsonConfigableUnconfigData (WxppInMsgMenuItemClickGeneral m a) = (MenuItemEventKeyHandleEnv m a)

    isNameOfInMsgHandler _ x =
        x == "menu-click-general:" <> menuItemEventKeyIdent (Proxy :: Proxy a)

    parseWithExtraData _ x1 _obj = return $ WxppInMsgMenuItemClickGeneral x1


type instance WxppInMsgProcessResult (WxppInMsgMenuItemClickGeneral m a) = WxppInMsgHandlerResult

instance (Monad m, MenuItemEventKeyHandle m a) =>
    IsWxppInMsgProcessor m (WxppInMsgMenuItemClickGeneral m a)
    where

    processInMsg (WxppInMsgMenuItemClickGeneral env) cache app_info bs ime = runExceptT $ do
        let in_msg = wxppInMessage ime
        liftM (fromMaybe []) $ do
            case in_msg of
                WxppInMsgEvent (WxppEvtClickItem evt_key) -> do
                    case parse menuItemEventKeyParser "" evt_key of
                        Left _err -> do
                            return Nothing

                        Right v -> do
                            liftM Just $ ExceptT $ handleMenuItemEventKey (v :: a) env cache app_info bs ime

                _ -> return Nothing



-- | Handler: 回复原文本消息中路径指定的任意消息
-- 为安全计，要保证文件的真实路径在约定的目录下
-- 另外，还要求设置一个简单的口令作为前缀，同时也作为识别
data WxppInMsgSendAsRequested = WxppInMsgSendAsRequested (NonEmpty FilePath) Text

instance JsonConfigable WxppInMsgSendAsRequested where
    type JsonConfigableUnconfigData WxppInMsgSendAsRequested = NonEmpty FilePath

    isNameOfInMsgHandler _ x = x == "as-you-request"

    parseWithExtraData _ msg_dirs obj =
        WxppInMsgSendAsRequested msg_dirs <$> obj .: "magic-word"


type instance WxppInMsgProcessResult WxppInMsgSendAsRequested = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgSendAsRequested
    where

    processInMsg (WxppInMsgSendAsRequested msg_dirs magic_word) cache app_info _bs ime = runExceptT $ do
        m_fp <- runMaybeT $ do
                let in_msg = wxppInMessage ime
                case in_msg of
                    WxppInMsgText content -> do
                        fp <- MaybeT $ return $
                                T.unpack . T.strip <$> T.stripPrefix (magic_word <> " ") content
                        let msg_dir = LNE.head msg_dirs
                        fp' <- liftIO $ canonicalizePath (msg_dir </> fp)
                        msg_dir' <- liftIO $ canonicalizePath msg_dir
                        guard $ splitDirectories msg_dir' `isPrefixOf` splitDirectories fp'
                        return fp

                    _ -> mzero

        case m_fp of
            Nothing -> return []
            Just fp -> do
                let get_atk = (tryWxppWsResultE "getting access token" $ liftIO $
                                    wxppCacheGetAccessToken cache app_id)
                                >>= maybe (throwE $ "no access token available") (return . fst)
                outmsg <- ExceptT $ runDelayedYamlLoaderL msg_dirs $ mkDelayedYamlLoader fp
                liftM (return . (True,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                                tryYamlExcE $ fromWxppOutMsgL msg_dirs cache get_atk outmsg

        where
          app_id = procAppIdInfoReceiverId app_info


-- | Handler: 回复用户的 OpenID 及 UnionID
data WxppInMsgShowWxppID = WxppInMsgShowWxppID

instance JsonConfigable WxppInMsgShowWxppID where
    type JsonConfigableUnconfigData WxppInMsgShowWxppID = ()

    isNameOfInMsgHandler _ x = x == "show-wxpp-id"

    parseWithExtraData _ _ _obj = return $ WxppInMsgShowWxppID


type instance WxppInMsgProcessResult WxppInMsgShowWxppID = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgShowWxppID
    where

    processInMsg WxppInMsgShowWxppID cache app_info _bs ime = runExceptT $ do
      atk <- (tryWxppWsResultE "getting access token" $ liftIO $
                  wxppCacheGetAccessToken cache app_id)
              >>= maybe (throwE $ "no access token available") (return . fst)
      let open_id = wxppInFromUserName ime
      qres <- tryWxppWsResultE "wxppQueryEndUserInfo" $
                  wxppQueryEndUserInfo atk open_id

      let txt = T.unlines [ "OpenID: " <> unWxppOpenID open_id
                          , "UnionID: " <>
                              (fromMaybe "" $
                                  fmap unWxppUnionID $ endUserQueryResultUnionID qres)
                          ]
      return $ return $
          ( True
          , Just $ WxppOutMsgText txt
          )

      where
        app_id = procAppIdInfoReceiverId app_info


type ForwardUrlMap = Map (WxppOpenID, WxppAppID) (UrlText, (UTCTime, Text))

-- | Handler: 用 JSON 格式转发(POST)收到的消息至另一个URL上
data WxppInMsgForwardAsJson = WxppInMsgForwardAsJson
                                    (MVar ForwardUrlMap)
                                    UrlText NominalDiffTime

instance JsonConfigable WxppInMsgForwardAsJson where
    type JsonConfigableUnconfigData WxppInMsgForwardAsJson = MVar ForwardUrlMap

    isNameOfInMsgHandler _ x = x == "forward-as-json"

    parseWithExtraData _ mvar obj =
        parseForwardData (WxppInMsgForwardAsJson mvar) obj

type instance WxppInMsgProcessResult WxppInMsgForwardAsJson = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgForwardAsJson
    where

    processInMsg (WxppInMsgForwardAsJson mvar url ttl) cache app_info _bs ime = runExceptT $ do
        atk <- (tryWxppWsResultE "getting access token" $ liftIO $
                    wxppCacheGetAccessToken cache app_id)
                >>= maybe (throwE $ "no access token available") (return . fst)
        let opts = defaults
            open_id = wxppInFromUserName ime
        qres <- tryWxppWsResultE "wxppCachedQueryEndUserInfo" $
                    wxppCachedQueryEndUserInfo cache ttl atk open_id
        let fwd_env = WxppForwardedEnv qres atk
        let fwd_msg = (ime, fwd_env)
        (r, m_exp) <- ((liftIO $ postWith opts (T.unpack $ unUrlText url) $ toJSON fwd_msg)
                >>= liftM (view responseBody) . asJSON)
                `catchAll` handle_exc

        -- 对方返回会话的过期时间如果是 Nothing，代表会话结束
        case m_exp of
            Nothing -> do
                liftIO $ modifyMVar_ mvar $
                  (return $!!) . Map.delete (open_id, app_id)
            Just (exp_t, timeout_txt) -> do
                liftIO $ modifyMVar_ mvar $
                  (return $!!) . Map.insert
                                            (open_id, app_id)
                                            (url, (exp_t, timeout_txt))

        return r
        where
            handle_exc ex =
                throwE $ "Failed to forward incoming message: " ++ show ex

            app_id = procAppIdInfoReceiverId app_info


-- | Handler: 用 JSON 格式转发(POST)收到的消息至另一个URL上
-- 仅当 MVar 当时有值时才执行，否则就不工作
data WxppInMsgForwardDyn = WxppInMsgForwardDyn
                                (MVar ForwardUrlMap)
                                NominalDiffTime

instance JsonConfigable WxppInMsgForwardDyn where
    type JsonConfigableUnconfigData WxppInMsgForwardDyn = MVar ForwardUrlMap

    isNameOfInMsgHandler _ x = x == "forward-dynamically"

    parseWithExtraData _ mvar obj = WxppInMsgForwardDyn mvar
                                    <$> ((fromIntegral :: Int -> NominalDiffTime)
                                            <$> obj .:? "user-info-cache-ttl" .!= (3600 * 2))


type instance WxppInMsgProcessResult WxppInMsgForwardDyn = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgForwardDyn
    where

    processInMsg (WxppInMsgForwardDyn mvar ttl) cache app_info bs ime = runExceptT $ do
        let open_id = wxppInFromUserName ime
        mx <- liftIO $ withMVar mvar $ return . Map.lookup (open_id, app_id)
        case mx of
            Nothing -> return []

            Just (url, (expire_time, _)) -> do
                now <- liftIO getCurrentTime
                if now >= expire_time
                    then return []
                    else ExceptT $ processInMsg
                                        (WxppInMsgForwardAsJson mvar url ttl)
                                        cache app_info bs ime
        where
          app_id = procAppIdInfoReceiverId app_info


-- | Handler: 把各种带有 WxppScene 事件中的 WxppScene 用 HTTP POST 转发至指定的 URL
data WxppInMsgForwardScene = WxppInMsgForwardScene
                                    (MVar ForwardUrlMap)
                                    UrlText
                                    NominalDiffTime

instance JsonConfigable WxppInMsgForwardScene where
    type JsonConfigableUnconfigData WxppInMsgForwardScene = MVar ForwardUrlMap

    isNameOfInMsgHandler _ x = x == "forward-scene"

    parseWithExtraData _ mvar obj =
        parseForwardData (WxppInMsgForwardScene mvar) obj


type instance WxppInMsgProcessResult WxppInMsgForwardScene = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgForwardScene
    where

    processInMsg (WxppInMsgForwardScene mvar url ttl) cache app_info _bs ime = runExceptT $ do
        case getEventInMsg (wxppInMessage ime) >>= getSceneInEvent of
            Nothing -> return []

            Just (scene, ticket) -> do
                atk <- (tryWxppWsResultE "getting access token" $ liftIO $
                            wxppCacheGetAccessToken cache app_id)
                        >>= maybe (throwE $ "no access token available") (return . fst)
                let opts = defaults
                    open_id = wxppInFromUserName ime
                qres <- tryWxppWsResultE "wxppCachedQueryEndUserInfo" $
                            wxppCachedQueryEndUserInfo cache ttl atk open_id
                let fwd_env = WxppForwardedEnv qres atk
                let fwd_msg = ((scene, ticket), fwd_env)
                (r, m_exp) <- ((liftIO $ postWith opts (T.unpack $ unUrlText url) $ toJSON fwd_msg)
                                >>= liftM (view responseBody) . asJSON)
                                `catchAll` handle_exc

                -- 对方返回会话的过期时间如果是 Nothing，代表会话结束
                case m_exp of
                    Nothing -> do
                        liftIO $ modifyMVar_ mvar $
                          (return $!!) . Map.delete (open_id, app_id)
                    Just (exp_t, timeout_txt) -> do
                        liftIO $ modifyMVar_ mvar $
                          (return $!!) . Map.insert
                                            (open_id, app_id)
                                            (url, (exp_t, timeout_txt))

                return r
        where
            handle_exc ex =
                throwE $ "Failed to forward incoming message: " ++ show ex

            app_id = procAppIdInfoReceiverId app_info


-- | Handler: 根据所带的 Predictor 与 Handler 对应表，分发到不同的 Handler 处理收到消息
data WxppInMsgDispatchHandler m =
        WxppInMsgDispatchHandler [(SomeWxppInMsgPredictor m, SomeWxppInMsgHandler m)]

instance Show (WxppInMsgDispatchHandler m) where
    show _ = "WxppInMsgDispatchHandler"

instance JsonConfigable (WxppInMsgDispatchHandler m) where
    type JsonConfigableUnconfigData (WxppInMsgDispatchHandler m) =
            ( [WxppInMsgPredictorPrototype m]
            , [WxppInMsgHandlerPrototype m]
            )

    isNameOfInMsgHandler _ x = x == "dispatch"

    parseWithExtraData _ (proto_pred, proto_handler) obj = do
        fmap WxppInMsgDispatchHandler $
            obj .: "route" >>= parseArray "message handler routes" parse_one
        where
            parse_one = withObject "message handler route" $ \o -> do
                p <- o .: "predictor" >>= parseWxppInMsgProcessor proto_pred
                h <- o .: "handler" >>= parseWxppInMsgProcessor proto_handler
                return $ (p, h)


type instance WxppInMsgProcessResult (WxppInMsgDispatchHandler m) = WxppInMsgHandlerResult

instance (Monad m, MonadLogger m) => IsWxppInMsgProcessor m (WxppInMsgDispatchHandler m)
    where
      processInMsg (WxppInMsgDispatchHandler table) cache app_info bs ime =
        go table
        where
            go []               = return $ Right []
            go ((p, h):xs)      = do
                err_or_b <- processInMsg p cache app_info bs ime
                b <- case err_or_b of
                    Left err -> do
                                $(logErrorS) wxppLogSource $ fromString $
                                    "Predictor failed: " <> err
                                return False
                    Right b -> return b
                if b
                    then processInMsg h cache app_info bs ime
                    else go xs


-- | Handler: 根据一个 YAML 的定义的表，找出可能配置的 article，打包是一个 news 消息返回
-- 用户输入的字串作为查找关键字，以完全匹配的方式匹配
data WxppMatchedKeywordArticles = WxppMatchedKeywordArticles
                                    (NonEmpty FilePath) -- msg dir
                                    Bool        -- if primary
                                    FilePath    -- the YAML

newtype ArtcileToKeywordsMap = ArtcileToKeywordsMap { unArtcileToKeywordsMap :: Map FilePath [Text] }

instance FromJSON ArtcileToKeywordsMap where
    parseJSON = withObject "ArtcileToKeywordsMap" $
        \obj -> fmap ArtcileToKeywordsMap $ do
                base_dir <- T.unpack <$> obj .:? "base" .!= "."
                ps <- obj .: "articles" >>= parseArray "[artcile-info]" parse_item
                return $ Map.fromList $ map (\(x, y) -> (base_dir </> x, y)) ps
            where
                parse_item = withObject "article-info" $ \o -> do
                    (,) <$> (T.unpack <$> o .: "file")
                        <*> (o .: "keywords")

instance JsonConfigable WxppMatchedKeywordArticles where
    -- | 不可配置部分是 out-msg 目录路径
    type JsonConfigableUnconfigData WxppMatchedKeywordArticles = NonEmpty FilePath

    isNameOfInMsgHandler _ x = x == "articles-match-keyword"

    parseWithExtraData _ msg_dirs obj = WxppMatchedKeywordArticles msg_dirs
                                    <$> ( obj .:? "primary" .!= False )
                                    <*> ( T.unpack <$> obj .: "map-file" )

type instance WxppInMsgProcessResult WxppMatchedKeywordArticles = WxppInMsgHandlerResult

instance (Monad m, Functor m, MonadIO m) => IsWxppInMsgProcessor m WxppMatchedKeywordArticles
    where
      processInMsg (WxppMatchedKeywordArticles msg_dirs is_primary map_file) _cache _app_info _bs ime = runExceptT $ do
        let in_msg = wxppInMessage ime
        let m_keyword = do
                case in_msg of
                    WxppInMsgText content   -> return content
                    _                       -> Nothing

        case m_keyword of
            Nothing -> return []
            Just keyword -> do
                ArtcileToKeywordsMap the_map <-
                        withExceptT show $ ExceptT $
                            runDelayedYamlLoaderL msg_dirs $ mkDelayedYamlLoader map_file

                let files = catMaybes $ flip map (Map.toList the_map) $
                                \(art_file, keywords) ->
                                    let matched = isJust $ find (== keyword) keywords
                                    in if matched
                                        then Just art_file
                                        else Nothing

                if null files
                    then return []
                    else do
                        articles <- forM files $ \article_file -> do
                                        ExceptT $ runDelayedYamlLoaderL msg_dirs
                                                        (mkDelayedYamlLoader $
                                                            setExtIfNotExist "yml" article_file)
                        let outmsg = WxppOutMsgNews $ take 10 articles
                        return $ return $ (is_primary, Just outmsg)


-- | Handler: 将满足某些条件的用户信息转发至微信的“多客服”系统
data TransferToCS = TransferToCS Bool
                    deriving (Show, Typeable)

instance JsonConfigable TransferToCS where
    type JsonConfigableUnconfigData TransferToCS = ()

    isNameOfInMsgHandler _ x = x == "transfer-to-cs"

    parseWithExtraData _ _ obj = TransferToCS <$> obj .:? "primary" .!= False


type instance WxppInMsgProcessResult TransferToCS = WxppInMsgHandlerResult

instance (Monad m) => IsWxppInMsgProcessor m TransferToCS where
    processInMsg (TransferToCS is_primary) _cache _app_info _bs _ime =
        return $ Right $ return $ (is_primary,) $ Just WxppOutMsgTransferToCustomerService


-- | Predictor: 判断信息是否是指定列表里的字串之一
-- 注意：用户输入去除空白之后，必须完整地匹配列表中某个元素才算匹配
data WxppInMsgMatchOneOf = WxppInMsgMatchOneOf [Text]
                            deriving (Show, Typeable)


instance JsonConfigable WxppInMsgMatchOneOf where
    type JsonConfigableUnconfigData WxppInMsgMatchOneOf = ()

    isNameOfInMsgHandler _ x = x == "one-of"

    parseWithExtraData _ _ obj = (WxppInMsgMatchOneOf . map T.strip) <$> obj .: "texts"


type instance WxppInMsgProcessResult WxppInMsgMatchOneOf = Bool

instance (Monad m) => IsWxppInMsgProcessor m WxppInMsgMatchOneOf where
    processInMsg (WxppInMsgMatchOneOf lst) _cache _app_info _bs ime = runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgText t -> return $ T.strip t `elem` lst
            _               -> return False

-- | Predictor: 判断信息是否是指定列表里的字串之一
-- 注意：用户输入去除空白之后，必须完整地匹配列表中某个元素才算匹配
data WxppInMsgMatchOneOfRe = WxppInMsgMatchOneOfRe [Regex]
                            deriving (Typeable)

instance Show WxppInMsgMatchOneOfRe where
    show (WxppInMsgMatchOneOfRe res) =
        "WxppInMsgMatchOneOfRe " ++ show (map examineDFA res)

instance JsonConfigable WxppInMsgMatchOneOfRe where
    type JsonConfigableUnconfigData WxppInMsgMatchOneOfRe = ()

    isNameOfInMsgHandler _ x = x == "one-of-posix-re"

    parseWithExtraData _ _ obj = do
        re_list <- obj .: "re"
        fmap WxppInMsgMatchOneOfRe $ mapM parsePosixRE re_list

type instance WxppInMsgProcessResult WxppInMsgMatchOneOfRe = Bool

instance (Monad m) => IsWxppInMsgProcessor m WxppInMsgMatchOneOfRe where
    processInMsg (WxppInMsgMatchOneOfRe lst) _cache _app_info _bs ime = runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgText t'  -> do
                let t = T.unpack $ T.strip t'
                return $ testWithPosixREList lst t

            _                 -> return False


-- | Predictor: 只要是文本消息就通过
-- 主要用于转发：通常转发只转发文本。
-- 而为了不在本地重复下游服务器的逻辑，全部转发也是一种合理的选择。
data WxppInMsgAnyText = WxppInMsgAnyText
                    deriving (Show, Typeable)

instance JsonConfigable WxppInMsgAnyText where
    type JsonConfigableUnconfigData WxppInMsgAnyText = ()

    isNameOfInMsgHandler _ x = x == "any-text"

    parseWithExtraData _ _ _obj = return WxppInMsgAnyText

type instance WxppInMsgProcessResult WxppInMsgAnyText = Bool

instance (Monad m) => IsWxppInMsgProcessor m WxppInMsgAnyText where
    processInMsg WxppInMsgAnyText _cache _app_info _bs ime = runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgText {} -> return True
            _                -> return False


-- | Predictor: 通过条件： scancode_waitmsg 事件，且 key 满足任意一个正则表达式
data WxppInMsgScanCodeWaitMsgKeyRE = WxppInMsgScanCodeWaitMsgKeyRE [Regex]
                                    deriving (Typeable)

instance JsonConfigable WxppInMsgScanCodeWaitMsgKeyRE where
    type JsonConfigableUnconfigData WxppInMsgScanCodeWaitMsgKeyRE = ()

    isNameOfInMsgHandler _ x = x == "scancode-wait-msg-key-re"

    parseWithExtraData _ _ obj = do
        re_list <- obj .: "key-re"
        fmap WxppInMsgScanCodeWaitMsgKeyRE $ mapM parsePosixRE re_list

type instance WxppInMsgProcessResult WxppInMsgScanCodeWaitMsgKeyRE = Bool

instance (Monad m) => IsWxppInMsgProcessor m WxppInMsgScanCodeWaitMsgKeyRE where
    processInMsg (WxppInMsgScanCodeWaitMsgKeyRE lst) _cache _app_info _bs ime = runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgEvent (WxppEvtScanCodeWaitMsg ek _scan_type _scan_result)  -> do
                let t = T.unpack ek
                return $ testWithPosixREList lst t

            _   -> return False


-- | Predictor: 通过条件：消息为带场景的事件推送，且场景满足正规表达式
data WxppInMsgSceneRE = WxppInMsgSceneRE [Regex]
                        deriving (Typeable)

instance JsonConfigable WxppInMsgSceneRE where
    type JsonConfigableUnconfigData WxppInMsgSceneRE = ()

    isNameOfInMsgHandler _ x = x == "scene-match-re"

    parseWithExtraData _ _ obj = do
        re_list <- obj .: "re"
        fmap WxppInMsgSceneRE $ mapM parsePosixRE re_list

type instance WxppInMsgProcessResult WxppInMsgSceneRE = Bool

instance (Monad m) => IsWxppInMsgProcessor m WxppInMsgSceneRE where
    processInMsg (WxppInMsgSceneRE lst) _cache _app_info _bs ime = runExceptT $ do
        case return (wxppInMessage ime) >>= getEventInMsg >>= getSceneInEvent of
            Nothing -> return False

            Just (scene, _ticket) -> do
                case scene of
                    WxppSceneInt _      -> return False
                    WxppSceneStr (WxppStrSceneID str) ->
                        return $ testWithPosixREList lst $ T.unpack str


-- | Predictor: Logical AND of some predictors
data WxppInMsgLogicalAndPred m = WxppInMsgLogicalAndPred [ SomeWxppInMsgPredictor m ]

instance JsonConfigable (WxppInMsgLogicalAndPred m) where
    type JsonConfigableUnconfigData (WxppInMsgLogicalAndPred m) = [WxppInMsgPredictorPrototype m]

    isNameOfInMsgHandler _ x = x == "logical-and"

    parseWithExtraData _ proto_pred obj = do
        fmap WxppInMsgLogicalAndPred $
            obj .: "predictors"
                >>= parseArray "predictors"
                        (withObject "predictor" $ parseWxppInMsgProcessor proto_pred)

type instance WxppInMsgProcessResult (WxppInMsgLogicalAndPred m) = Bool

instance (Monad m) =>
    IsWxppInMsgProcessor m (WxppInMsgLogicalAndPred m)
    where

    processInMsg (WxppInMsgLogicalAndPred preds) cache app_info bs ime = runExceptT $ do
        liftM (all id) $ forM preds $ \p -> ExceptT $ processInMsg p cache app_info bs ime


-- | Predictor: Logical OR of some predictors
data WxppInMsgLogicalOrPred m = WxppInMsgLogicalOrPred [ SomeWxppInMsgPredictor m ]

instance JsonConfigable (WxppInMsgLogicalOrPred m) where
    type JsonConfigableUnconfigData (WxppInMsgLogicalOrPred m) = [WxppInMsgPredictorPrototype m]

    isNameOfInMsgHandler _ x = x == "logical-or"

    parseWithExtraData _ proto_pred obj = do
        fmap WxppInMsgLogicalOrPred $
            obj .: "predictors"
                >>= parseArray "predictors"
                        (withObject "predictor" $ parseWxppInMsgProcessor proto_pred)

type instance WxppInMsgProcessResult (WxppInMsgLogicalOrPred m) = Bool

instance (Monad m) =>
    IsWxppInMsgProcessor m (WxppInMsgLogicalOrPred m)
    where

    processInMsg (WxppInMsgLogicalOrPred preds) cache app_info bs ime = runExceptT $ go preds
        where
            go []       = return False
            go (p:ps)   = do
                            b <- ExceptT $ processInMsg p cache app_info bs ime
                            if b
                                then return True
                                else go ps


-- | Handler: 固定地返回一个某个信息
data ConstResponse = ConstResponse (NonEmpty FilePath) Bool WxppOutMsgLoader
                    deriving (Typeable)

instance JsonConfigable ConstResponse where
    type JsonConfigableUnconfigData ConstResponse = NonEmpty FilePath

    isNameOfInMsgHandler _ x = x == "const"

    parseWithExtraData _ msg_dirs obj = do
        liftM2 (ConstResponse msg_dirs)
            (obj .:? "primary" .!= False)
            (parseWxppOutMsgLoader obj)


type instance WxppInMsgProcessResult ConstResponse = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadCatch m) =>
    IsWxppInMsgProcessor m ConstResponse
    where

    processInMsg (ConstResponse msg_dirs is_primary get_outmsg) cache app_info _bs _ime = runExceptT $ do
        let get_atk = (tryWxppWsResultE "getting access token" $ liftIO $
                            wxppCacheGetAccessToken cache app_id)
                        >>= maybe (throwE $ "no access token available") (return . fst)
        outmsg <- ExceptT $ runDelayedYamlLoaderL msg_dirs get_outmsg
        liftM (return . (is_primary,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                        tryYamlExcE $ fromWxppOutMsgL msg_dirs cache get_atk outmsg
        where
          app_id = procAppIdInfoReceiverId app_info


-- | Handler: 解释菜单扫描的二维码事件, 对应菜单事件的 scancode_msg
data WxppInMsgParseScanCodePush m a = WxppInMsgParseScanCodePush
                                        (Text -> m (Either String a))
                                            -- parser for command
                                        (Text       -- event key
                                            -> WxppInMsgEntity
                                            -> a
                                            -> m (Either String WxppInMsgHandlerResult)
                                        )
                                        -- handle the command

instance JsonConfigable (WxppInMsgParseScanCodePush m a) where
    type JsonConfigableUnconfigData (WxppInMsgParseScanCodePush m a) =
            ( Text -> m (Either String a)
            , Text -> WxppInMsgEntity -> a -> m (Either String WxppInMsgHandlerResult)
            )

    isNameOfInMsgHandler _ x = x == "generic-scancode-push"

    parseWithExtraData _ (f1, f2) _obj = return $ WxppInMsgParseScanCodePush f1 f2

type instance WxppInMsgProcessResult (WxppInMsgParseScanCodePush m a) = WxppInMsgHandlerResult

instance (WxppApiMonad env m) =>
    IsWxppInMsgProcessor m (WxppInMsgParseScanCodePush m a)
    where

    processInMsg (WxppInMsgParseScanCodePush parse_f handle_f) _cache _app_info _bs ime = runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgEvent (WxppEvtScanCodePush ev_key _scan_type scan_txt) -> do
                -- scancode_push 事件好像不能直接回复信息
                -- 所以，如果结果非空，就在前面多加一个空的回复
                let fix_out xs =
                        if any fst xs
                            then (True, Just (WxppOutMsgText "")) : xs
                            else xs
                (ExceptT $ parse_f scan_txt)
                    >>= (ExceptT . handle_f ev_key ime)
                    >>= return . fix_out

            _ -> return []


-- | Handler: 解释菜单扫描的二维码事件, 对应菜单事件的 scancode_waitmsg
data WxppInMsgParseScanCodeWaitMsg m a = WxppInMsgParseScanCodeWaitMsg
                                        (Text -> m (Either String a))
                                            -- parser for command
                                        (Text       -- event key
                                            -> WxppInMsgEntity
                                            -> a
                                            -> m (Either String WxppInMsgHandlerResult)
                                        )
                                        -- handle the command

instance JsonConfigable (WxppInMsgParseScanCodeWaitMsg m a) where
    type JsonConfigableUnconfigData (WxppInMsgParseScanCodeWaitMsg m a) =
            ( Text -> m (Either String a)
            , Text -> WxppInMsgEntity -> a -> m (Either String WxppInMsgHandlerResult)
            )

    isNameOfInMsgHandler _ x = x == "generic-scancode-waitmsg"

    parseWithExtraData _ (f1, f2) _obj = return $ WxppInMsgParseScanCodeWaitMsg f1 f2

type instance WxppInMsgProcessResult (WxppInMsgParseScanCodeWaitMsg m a) = WxppInMsgHandlerResult

instance (WxppApiMonad env m) =>
    IsWxppInMsgProcessor m (WxppInMsgParseScanCodeWaitMsg m a)
    where

    processInMsg (WxppInMsgParseScanCodeWaitMsg parse_f handle_f) _cache _app_info _bs ime =
      runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgEvent (WxppEvtScanCodeWaitMsg ev_key _scan_type scan_txt) -> do
                (ExceptT $ parse_f scan_txt)
                    >>= (ExceptT . handle_f ev_key ime)

            _ -> return []


-- | 直接包装一个函数，相当于可以用任意类型适合的函数处理消息
newtype WxppInMsgProcessorFunc m h = WxppInMsgProcessorFunc
                                        (forall c. (WxppCacheTokenReader c, WxppCacheTemp c) =>
                                            c
                                            -> WxppInMsgProcessor m h
                                        )

type instance WxppInMsgProcessResult (WxppInMsgProcessorFunc m h) = h

instance IsWxppInMsgProcessor m (WxppInMsgProcessorFunc m h) where
    processInMsg (WxppInMsgProcessorFunc f) = f


-- | 用于解释 SomeWxppInMsgHandler 的类型信息
-- 结果中不包含 WxppInMsgParseScanCodePush, WxppInMsgParseScanCodeWaitMsg
-- 因为不想传入太多不相关的参数
allBasicWxppInMsgHandlerPrototypes :: ( WxppApiMonad env m, MonadCatch m )
                                   => NonEmpty FilePath
                                   -> MVar ForwardUrlMap
                                   -> [WxppInMsgHandlerPrototype m]
allBasicWxppInMsgHandlerPrototypes msg_dirs mvar =
    [ WxppInMsgProcessorPrototype (Proxy :: Proxy WelcomeSubscribe) (msg_dirs)
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgMenuItemClickSendMsg) (msg_dirs)
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgSendAsRequested) (msg_dirs)
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppMatchedKeywordArticles) msg_dirs
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgForwardAsJson) (mvar)
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgForwardScene) (mvar)
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgForwardDyn) (mvar)
    , WxppInMsgProcessorPrototype (Proxy :: Proxy TransferToCS) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy ConstResponse) (msg_dirs)
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgShowWxppID) ()
    ]


-- | 用于解释 SomeWxppInMsgPredictor 的类型信息
allBasicWxppInMsgPredictorPrototypes ::
    ( WxppApiMonad env m ) =>
    [WxppInMsgPredictorPrototype m]
allBasicWxppInMsgPredictorPrototypes =
    [ WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgMatchOneOf) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgMatchOneOfRe) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgScanCodeWaitMsgKeyRE) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgSceneRE) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgAnyText) ()
    ]

--------------------------------------------------------------------------------

parseForwardData ::
    (UrlText -> NominalDiffTime -> a)   -- ^ the constructor
    -> Object
    -> Parser a
parseForwardData ctor obj =
    ctor    <$> (UrlText <$> obj .: "url")
            <*> ((fromIntegral :: Int -> NominalDiffTime)
                    <$> obj .:? "user-info-cache-ttl" .!= (3600 * 2))


parsePosixRE :: Monad m => String -> m Regex
parsePosixRE r = do
    case compile blankCompOpt blankExecOpt r of
        Left err -> fail $ "Failed to compile RE: " <> err
        Right rx -> return rx

testWithPosixREList :: [Regex] -> String -> Bool
testWithPosixREList lst t = not $ null $ catMaybes $ rights $ map (flip execute t) lst

-- | 取事件推送消息中可能带有的场景信息
getSceneInEvent :: WxppEvent -> Maybe (WxppScene, Maybe QRTicket)
getSceneInEvent (WxppEvtSubscribeAtScene scene ticket)  = Just (scene, ticket)
getSceneInEvent (WxppEvtScan scene ticket)              = Just (scene, ticket)
getSceneInEvent _                                       = Nothing

getEventInMsg :: WxppInMsg -> Maybe WxppEvent
getEventInMsg (WxppInMsgEvent x)    = Just x
getEventInMsg _                     = Nothing

