{-# LANGUAGE TupleSections #-}
module WeiXin.PublicPlatform.InMsgHandler where

import ClassyPrelude hiding (catch)
import Network.Wreq hiding (Proxy)
import Control.Lens hiding ((<.>), op)
import Data.Proxy
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Monad.Catch                  ( catchAll )
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Map.Strict            as Map
import Data.Aeson
import Data.Aeson.Types                     (Parser)
import Data.Yaml                            (decodeFileEither, parseEither, ParseException(..))
import Data.Time                            (NominalDiffTime)

import Text.Regex.TDFA                      (blankExecOpt, blankCompOpt, Regex)
import Text.Regex.TDFA.TDFA                 ( examineDFA)
import Text.Regex.TDFA.String               (compile, execute)
import Filesystem.Path.CurrentOS            (fromText)
import qualified Filesystem.Path.CurrentOS  as FP
import Control.Monad.Catch                  (catch)

import Yesod.Helpers.Aeson                  (parseArray)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Media
import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.EndUser
import WeiXin.PublicPlatform.Utils


-- | 对收到的消息处理的函数
type WxppInMsgProcessor m a =
        LB.ByteString
            -- ^ raw data of message (unparsed)
        -> Maybe WxppInMsgEntity
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


-- | 预处理收到的消息的结果
type family WxppInMsgProcessResult h :: *


-- | 对收到的消息作出某种处理
-- 实例分为两大类：
-- Predictor 其处理结果是个 Bool 值
-- Handler 其处理结果是个 WxppInMsgHandlerResult
class IsWxppInMsgProcessor m h where
    processInMsg ::
        h
        -> AcidState WxppAcidState
        -> m (Maybe AccessToken)
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
    -> String
    -> IO (Either ParseException [SomeWxppInMsgHandler m])
readWxppInMsgHandlers tmps fp = runExceptT $ do
    (ExceptT $ decodeFileEither fp)
        >>= either (throwE . AesonException) return
                . parseEither (parseWxppInMsgProcessors tmps)


-- | 使用列表里的所有算法，逐一调用一次以处理收到的信息
tryEveryInMsgHandler :: MonadLogger m =>
    [WxppInMsgHandler m] -> WxppInMsgHandler m
tryEveryInMsgHandler handlers bs m_ime = do
    (errs, res_lst) <- liftM partitionEithers $
                            mapM (\h -> h bs m_ime) handlers
    forM_ errs $ \err -> do
        $(logWarnS) wxppLogSource $ T.pack $
            "Error when handling incoming message, "
            <> "MsgId=" <> (show $ join $ fmap wxppInMessageID m_ime)
            <> ": " <> err

    return $ Right $ join res_lst


tryEveryInMsgHandler' :: MonadLogger m =>
    AcidState WxppAcidState
    -> m (Maybe AccessToken)
    -> [SomeWxppInMsgHandler m]
    -> WxppInMsgHandler m
tryEveryInMsgHandler' acid get_atk known_hs = do
    tryEveryInMsgHandler $ flip map known_hs $ \h -> processInMsg h acid get_atk

tryWxppWsResultE :: MonadCatch m =>
    String -> ExceptT String m b -> ExceptT String m b
tryWxppWsResultE op f =
    tryWxppWsResult f
        >>= either (\e -> throwE $ "Got Exception when " <> op <> ": " <> show e) return

tryYamlExcE :: MonadCatch m => ExceptT String m b -> ExceptT String m b
tryYamlExcE f =
    f `catch` (\e -> throwE $ show (e :: ParseException))

parseWxppOutMsgLoader :: Object -> Parser WxppOutMsgLoader
parseWxppOutMsgLoader obj = do
    -- parseDelayedYamlLoader (Just "msg") "file"
    -- 逐一尝试以下字段
    -- msg: 内嵌表示的完整消息
    -- artcile-file: 外部文件定义的单图文消息
    -- file: 外部文件定义的完整消息
    parse_direct
        <|> (fmap (fmap $ \x -> WxppOutMsgNewsL [ return (Right x) ] ) <$> parse_indirect1)
        <|> parse_indirect2
    where
        parse_indirect1 = mkDelayedYamlLoader . setExtIfNotExist "yml" . fromText <$> obj .: "article-file"
        parse_indirect2 = mkDelayedYamlLoader . setExtIfNotExist "yml" . fromText <$> obj .: "file"
        parse_direct    = return . Right <$> obj .: "msg"


-- | Handler: 处理收到的信息的算法例子：用户订阅公众号时发送欢迎信息
data WelcomeSubscribe = WelcomeSubscribe
                            FilePath            -- ^ 所有消息文件存放的目录
                            WxppOutMsgLoader    -- ^ 打算回复用户的消息
                        deriving (Typeable)

instance JsonConfigable WelcomeSubscribe where
    type JsonConfigableUnconfigData WelcomeSubscribe = FilePath

    isNameOfInMsgHandler _ x = x == "welcome-subscribe"

    parseWithExtraData _ msg_dir obj = do
        WelcomeSubscribe msg_dir <$> parseWxppOutMsgLoader obj


type instance WxppInMsgProcessResult WelcomeSubscribe = WxppInMsgHandlerResult

instance (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    IsWxppInMsgProcessor m WelcomeSubscribe
    where

    processInMsg (WelcomeSubscribe msg_dir get_outmsg) acid get_atk _bs m_ime = runExceptT $ do
        is_subs <- case fmap wxppInMessage m_ime of
                    Just (WxppInMsgEvent WxppEvtSubscribe)              -> return True
                    Just (WxppInMsgEvent (WxppEvtSubscribeAtScene {}))  -> return True
                    _                                                   -> return False
        if is_subs
            then do
                atk <- (tryWxppWsResultE "getting access token" $ lift get_atk)
                        >>= maybe (throwE $ "no access token available") return
                outmsg <- ExceptT $ runDelayedYamlLoader msg_dir get_outmsg
                liftM (return . (True,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                                tryYamlExcE $ fromWxppOutMsgL msg_dir acid atk outmsg
            else return []


-- | Handler: 处理点击菜单项目的事件通知，加载 Key 参数中指定的文件所记录的消息
-- 要求 Key 参数的格式为： send-msg:<path to yaml>
data WxppInMsgMenuItemClickSendMsg = WxppInMsgMenuItemClickSendMsg FilePath

instance JsonConfigable WxppInMsgMenuItemClickSendMsg where
    type JsonConfigableUnconfigData WxppInMsgMenuItemClickSendMsg = FilePath

    isNameOfInMsgHandler _ x = x == "menu-click-send-msg"

    parseWithExtraData _ msg_dir _obj = return $ WxppInMsgMenuItemClickSendMsg msg_dir


type instance WxppInMsgProcessResult WxppInMsgMenuItemClickSendMsg = WxppInMsgHandlerResult

instance (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgMenuItemClickSendMsg
    where

    processInMsg (WxppInMsgMenuItemClickSendMsg msg_dir) acid get_atk _bs m_ime = runExceptT $ do
        let m_fp = do
                in_msg <- fmap wxppInMessage m_ime
                case in_msg of
                    WxppInMsgEvent (WxppEvtClickItem evt_key) -> do
                            T.stripPrefix "send-msg:" evt_key

                    _ -> Nothing

        case m_fp of
            Nothing -> return []
            Just fp' -> do
                atk <- (tryWxppWsResultE "getting access token" $ lift get_atk)
                        >>= maybe (throwE $ "no access token available") return
                let fp = setExtIfNotExist "yml" $ fromText fp'
                outmsg <- ExceptT $ runDelayedYamlLoader msg_dir $ mkDelayedYamlLoader fp
                liftM (return . (True,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                                tryYamlExcE $ fromWxppOutMsgL msg_dir acid atk outmsg


-- | Handler: 回复原文本消息中路径指定的任意消息
-- 为安全计，要保证文件的真实路径在约定的目录下
-- 另外，还要求设置一个简单的口令作为前缀，同时也作为识别
data WxppInMsgSendAsRequested = WxppInMsgSendAsRequested FilePath Text

instance JsonConfigable WxppInMsgSendAsRequested where
    type JsonConfigableUnconfigData WxppInMsgSendAsRequested = FilePath

    isNameOfInMsgHandler _ x = x == "as-you-request"

    parseWithExtraData _ msg_dir obj = WxppInMsgSendAsRequested msg_dir <$> obj .: "magic-word"


type instance WxppInMsgProcessResult WxppInMsgSendAsRequested = WxppInMsgHandlerResult

instance (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgSendAsRequested
    where

    processInMsg (WxppInMsgSendAsRequested msg_dir magic_word) acid get_atk _bs m_ime = runExceptT $ do
        let m_fp = do
                in_msg <- fmap wxppInMessage m_ime
                case in_msg of
                    WxppInMsgText content -> do
                        fp <- fromText . T.strip <$> T.stripPrefix (magic_word <> " ") content
                        fp2 <- FP.stripPrefix msg_dir (FP.collapse (msg_dir </> fp))
                        when (fp /= fp2) mzero
                        return fp

                    _ -> Nothing

        case m_fp of
            Nothing -> return []
            Just fp -> do
                atk <- (tryWxppWsResultE "getting access token" $ lift get_atk)
                        >>= maybe (throwE $ "no access token available") return
                outmsg <- ExceptT $ runDelayedYamlLoader msg_dir $ mkDelayedYamlLoader fp
                liftM (return . (True,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                                tryYamlExcE $ fromWxppOutMsgL msg_dir acid atk outmsg


-- | Handler: 用 JSON 格式转发(POST)收到的消息至另一个URL上
data WxppInMsgForwardAsJson = WxppInMsgForwardAsJson UrlText NominalDiffTime

instance JsonConfigable WxppInMsgForwardAsJson where
    type JsonConfigableUnconfigData WxppInMsgForwardAsJson = ()

    isNameOfInMsgHandler _ x = x == "forward-as-json"

    parseWithExtraData _ _ obj = parseForwardData WxppInMsgForwardAsJson obj


type instance WxppInMsgProcessResult WxppInMsgForwardAsJson = WxppInMsgHandlerResult

instance (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgForwardAsJson
    where

    processInMsg (WxppInMsgForwardAsJson url ttl) acid get_atk _bs m_ime = runExceptT $ do
        case m_ime of
            Nothing -> do
                $logWarnS wxppLogSource $
                    "Cannot forward incoming message as JSON because it could not be parsed."
                return []

            Just ime -> do
                atk <- (tryWxppWsResultE "getting access token" $ lift get_atk)
                        >>= maybe (throwE $ "no access token available") return
                let opts = defaults
                    open_id = wxppInFromUserName ime
                m_uid <- wxppCachedGetEndUserUnionID ttl acid atk open_id
                let fwd_env = WxppForwardedEnv m_uid atk
                let fwd_msg = (ime, fwd_env)
                ((liftIO $ postWith opts (T.unpack $ unUrlText url) $ toJSON fwd_msg)
                    >>= liftM (view responseBody) . asJSON)
                    `catchAll` handle_exc

        where
            handle_exc ex =
                throwE $ "Failed to forward incoming message: " ++ show ex


-- | Handler: 把各种带有 WxppScene 事件中的 WxppScene 用 HTTP POST 转发至指定的 URL
data WxppInMsgForwardScene = WxppInMsgForwardScene UrlText NominalDiffTime

instance JsonConfigable WxppInMsgForwardScene where
    type JsonConfigableUnconfigData WxppInMsgForwardScene = ()

    isNameOfInMsgHandler _ x = x == "forward-scene"

    parseWithExtraData _ _ obj = parseForwardData WxppInMsgForwardScene obj


type instance WxppInMsgProcessResult WxppInMsgForwardScene = WxppInMsgHandlerResult

instance (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppInMsgForwardScene
    where

    processInMsg (WxppInMsgForwardScene url ttl) acid get_atk _bs m_ime = runExceptT $ do
        case m_ime of
            Nothing -> do
                $logWarnS wxppLogSource $
                    "Cannot forward incoming message as JSON because it could not be parsed."
                return []

            Just ime -> do
                case getEventInMsg (wxppInMessage ime) >>= getSceneInEvent of
                    Nothing -> return []

                    Just (scene, ticket) -> do
                        atk <- (tryWxppWsResultE "getting access token" $ lift get_atk)
                                >>= maybe (throwE $ "no access token available") return
                        let opts = defaults
                            open_id = wxppInFromUserName ime
                        m_uid <- wxppCachedGetEndUserUnionID ttl acid atk open_id
                        let fwd_env = WxppForwardedEnv m_uid atk
                        let fwd_msg = ((scene, ticket), fwd_env)
                        ((liftIO $ postWith opts (T.unpack $ unUrlText url) $ toJSON fwd_msg)
                            >>= liftM (view responseBody) . asJSON)
                            `catchAll` handle_exc
        where
            handle_exc ex =
                throwE $ "Failed to forward incoming message: " ++ show ex


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
    processInMsg (WxppInMsgDispatchHandler table) acid get_atk bs m_ime =
        go table
        where
            go []               = return $ Right []
            go ((p, h):xs)      = do
                err_or_b <- processInMsg p acid get_atk bs m_ime
                b <- case err_or_b of
                    Left err -> do
                                $(logErrorS) wxppLogSource $ fromString $
                                    "Predictor failed: " <> err
                                return False
                    Right b -> return b
                if b
                    then processInMsg h acid get_atk bs m_ime
                    else go xs


-- | Handler: 根据一个 YAML 的定义的表，找出可能配置的 article，打包是一个 news 消息返回
-- 用户输入的字串作为查找关键字，以完全匹配的方式匹配
data WxppMatchedKeywordArticles = WxppMatchedKeywordArticles
                                    FilePath    -- ^ msg dir
                                    Bool        -- ^ if primary
                                    FilePath    -- ^ the YAML

newtype ArtcileToKeywordsMap = ArtcileToKeywordsMap { unArtcileToKeywordsMap :: Map FilePath [Text] }

instance FromJSON ArtcileToKeywordsMap where
    parseJSON = withObject "ArtcileToKeywordsMap" $
        \obj -> fmap ArtcileToKeywordsMap $ do
                base_dir <- fromText <$> obj .:? "base" .!= "."
                pairs <- obj .: "articles" >>= parseArray "[artcile-info]" parse_item
                return $ Map.fromList $ map (\(x, y) -> (base_dir </> x, y)) pairs
            where
                parse_item = withObject "article-info" $ \o -> do
                    (,) <$> (fromText <$> o .: "file")
                        <*> (o .: "keywords")

instance JsonConfigable WxppMatchedKeywordArticles where
    -- | 不可配置部分是 out-msg 目录路径
    type JsonConfigableUnconfigData WxppMatchedKeywordArticles = FilePath

    isNameOfInMsgHandler _ x = x == "articles-match-keyword"

    parseWithExtraData _ msg_dir obj = WxppMatchedKeywordArticles msg_dir
                                    <$> ( obj .:? "primary" .!= False )
                                    <*> ( fromText <$> obj .: "map-file" )

type instance WxppInMsgProcessResult WxppMatchedKeywordArticles = WxppInMsgHandlerResult

instance (Monad m, MonadLogger m, MonadIO m) => IsWxppInMsgProcessor m WxppMatchedKeywordArticles
    where
    processInMsg (WxppMatchedKeywordArticles msg_dir is_primary map_file) _acid _get_atk _bs m_ime = runExceptT $ do
        let m_keyword = do
                in_msg <- fmap wxppInMessage m_ime
                case in_msg of
                    WxppInMsgText content   -> return content
                    _                       -> Nothing

        case m_keyword of
            Nothing -> return []
            Just keyword -> do
                ArtcileToKeywordsMap the_map <-
                        (liftIO $ decodeFileEither (FP.encodeString $ msg_dir </> map_file))
                                >>= either (throwE . show) return
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
                                        ExceptT $ runDelayedYamlLoader msg_dir
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
    processInMsg (TransferToCS is_primary) _acid _get_atk _bs _m_ime =
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
    processInMsg (WxppInMsgMatchOneOf lst) _acid _get_atk _bs m_ime = runExceptT $ do
        case wxppInMessage <$> m_ime of
            Just (WxppInMsgText t)  -> return $ T.strip t `elem` lst
            _                       -> return False

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
    processInMsg (WxppInMsgMatchOneOfRe lst) _acid _get_atk _bs m_ime = runExceptT $ do
        case wxppInMessage <$> m_ime of
            Just (WxppInMsgText t')  -> do
                let t = T.unpack $ T.strip t'
                return $ testWithPosixREList lst t

            _                       -> return False


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
    processInMsg WxppInMsgAnyText _acid _get_atk _bs m_ime = runExceptT $ do
        case wxppInMessage <$> m_ime of
            Just (WxppInMsgText {}) -> return True
            _                       -> return False


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
    processInMsg (WxppInMsgScanCodeWaitMsgKeyRE lst) _acid _get_atk _bs m_ime = runExceptT $ do
        case wxppInMessage <$> m_ime of
            Just (WxppInMsgEvent (WxppEvtScanCodeWaitMsg ek _scan_type _scan_result))  -> do
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
    processInMsg (WxppInMsgSceneRE lst) _acid _get_atk _bs m_ime = runExceptT $ do
        case fmap wxppInMessage m_ime >>= getEventInMsg >>= getSceneInEvent of
            Nothing -> return False

            Just (scene, _ticket) -> do
                case scene of
                    WxppSceneInt _      -> return False
                    WxppSceneStr (WxppStrSceneID str) ->
                        return $ testWithPosixREList lst $ T.unpack str



-- | Handler: 固定地返回一个某个信息
data ConstResponse = ConstResponse FilePath Bool WxppOutMsgLoader
                    deriving (Typeable)

instance JsonConfigable ConstResponse where
    type JsonConfigableUnconfigData ConstResponse = FilePath

    isNameOfInMsgHandler _ x = x == "const"

    parseWithExtraData _ msg_dir obj = do
        liftM2 (ConstResponse msg_dir)
            (obj .:? "primary" .!= False)
            (parseWxppOutMsgLoader obj)


type instance WxppInMsgProcessResult ConstResponse = WxppInMsgHandlerResult

instance (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m) =>
    IsWxppInMsgProcessor m ConstResponse
    where

    processInMsg (ConstResponse msg_dir is_primary get_outmsg) acid get_atk _bs _m_ime = runExceptT $ do
        atk <- (tryWxppWsResultE "getting access token" $ lift get_atk)
                >>= maybe (throwE $ "no access token available") return
        outmsg <- ExceptT $ runDelayedYamlLoader msg_dir get_outmsg
        liftM (return . (is_primary,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                        tryYamlExcE $ fromWxppOutMsgL msg_dir acid atk outmsg


-- | 用于解释 SomeWxppInMsgHandler 的类型信息
allBasicWxppInMsgHandlerPrototypes ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m ) =>
    WxppAppID
    -> FilePath
    -> [WxppInMsgHandlerPrototype m]
allBasicWxppInMsgHandlerPrototypes _app_id msg_dir =
    [ WxppInMsgProcessorPrototype (Proxy :: Proxy WelcomeSubscribe) msg_dir
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgMenuItemClickSendMsg) msg_dir
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgSendAsRequested) msg_dir
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppMatchedKeywordArticles) msg_dir
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgForwardAsJson) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy WxppInMsgForwardScene) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy TransferToCS) ()
    , WxppInMsgProcessorPrototype (Proxy :: Proxy ConstResponse) msg_dir
    ]


-- | 用于解释 SomeWxppInMsgPredictor 的类型信息
allBasicWxppInMsgPredictorPrototypes ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m ) =>
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
                    <$> obj .:? "union-id-ttl" .!= (3600 * 24 * 365))


parsePosixRE :: Monad m => String -> m Regex
parsePosixRE r = do
    case compile blankCompOpt blankExecOpt r of
        Left err -> fail $ "Failed to compile RE: " <> err
        Right rx -> return rx

testWithPosixREList :: [Regex] -> String -> Bool
testWithPosixREList lst t = not $ null $ catMaybes $ rights $ map (flip execute t) lst

-- | 取事件推送消息中可能带有的场景信息
getSceneInEvent :: WxppEvent -> Maybe (WxppScene, QRTicket)
getSceneInEvent (WxppEvtSubscribeAtScene scene ticket)  = Just (scene, ticket)
getSceneInEvent (WxppEvtScan scene ticket)              = Just (scene, ticket)
getSceneInEvent _                                       = Nothing

getEventInMsg :: WxppInMsg -> Maybe WxppEvent
getEventInMsg (WxppInMsgEvent x)    = Just x
getEventInMsg _                     = Nothing

