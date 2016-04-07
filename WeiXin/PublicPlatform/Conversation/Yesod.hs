{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module WeiXin.PublicPlatform.Conversation.Yesod where

import ClassyPrelude.Yesod hiding (Proxy, proxy)
import Data.Proxy
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Text                  as T
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
import qualified Data.Conduit.List          as CL

import Control.Monad.Except hiding (forM_)
import Control.Monad.Trans.Maybe
import Data.Time                            (NominalDiffTime, addUTCTime)
import Data.List.NonEmpty                   (NonEmpty(..))
import Data.Aeson                           ((.:?), (.!=))

import WeiXin.PublicPlatform.Conversation
import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.Yesod.Model
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils
import WeiXin.PublicPlatform.Media


saveWxppTalkState :: forall m a r.
    ( MonadIO m, ToJSON a, WxTalkerState r (ReaderT SqlBackend m) a) =>
    (a -> Text)
    -> WxppTalkStateId
    -> a
    -> WxTalkerMonad r (ReaderT SqlBackend m) ()
saveWxppTalkState get_state_type state_id x = mkWxTalkerMonad $ \env -> runExceptT $ do
    now <- liftIO getCurrentTime
    done <- ExceptT $ flip runWxTalkerMonad env $ wxTalkIfDone x
    lift $ update state_id
            [ WxppTalkStateTyp          =. get_state_type x
            , WxppTalkStateJson         =. (LB.toStrict $ A.encode x)
            , WxppTalkStateDone         =. done
            , WxppTalkStateUpdatedTime  =. now
            ]


abortCurrentWxppTalkState :: forall m.
    (MonadIO m, MonadLogger m) =>
    WxppAppID
    -> WxppOpenID
    -> ReaderT SqlBackend m ()
abortCurrentWxppTalkState app_id open_id = do
    m_rec <- selectFirst
                        [ WxppTalkStateOpenId ==. open_id
                        , WxppTalkStateAppId ==. app_id
                        ]
                        [ Desc WxppTalkStateId ]
    forM_ m_rec $ \ (Entity rec_id _rec) -> do
        update rec_id [ WxppTalkStateAborted =. True ]


newWxppTalkState :: forall m a.
    ( MonadIO m, ToJSON a) =>
    (a -> Text)
    -> WxppAppID
    -> WxppOpenID
    -> a
    -> ReaderT SqlBackend m WxppTalkStateId
newWxppTalkState get_state_type app_id open_id x = do
    now <- liftIO getCurrentTime
    insert $ WxppTalkState
                app_id
                open_id
                (get_state_type x)
                (LB.toStrict $ A.encode x)
                False
                False
                now
                now

newWxppTalkState' :: forall m a.
    ( MonadIO m, HasStateType a, ToJSON a ) =>
    WxppAppID
    -> WxppOpenID
    -> a
    -> ReaderT SqlBackend m WxppTalkStateId
newWxppTalkState' = newWxppTalkState getStateType'


saveAnyWxppTalkState :: forall m r a.
    ( MonadIO m, ToJSON a, HasStateType a, WxTalkerState r (ReaderT WxppDbBackend m) a) =>
    WxppTalkStateId
    -> a
    -> WxTalkerMonad r (ReaderT WxppDbBackend m) ()
saveAnyWxppTalkState = saveWxppTalkState getStateType'

loadAnyWxppTalkState :: forall m a.
    (MonadIO m, MonadLogger m, FromJSON a, HasStateType a) =>
    Proxy a
    -> WxppTalkStateId
    -> ReaderT WxppDbBackend m (Either String (Maybe a))
loadAnyWxppTalkState proxy state_id = runExceptT $ runMaybeT $ do
    rec <- MaybeT $ lift $ get state_id
    MaybeT $ ExceptT $ parseWxppTalkStateFromRecord proxy $ Entity state_id rec

parseWxppTalkStateFromRecord :: forall m a.
    ( MonadLogger m, HasStateType a, FromJSON a ) =>
    Proxy a
    -> Entity WxppTalkState
    -> m (Either String (Maybe a))
parseWxppTalkStateFromRecord proxy (Entity rec_id rec) = runExceptT $ runMaybeT $ do
    when ( wxppTalkStateAborted rec ) mzero

    let state_type = wxppTalkStateTyp rec
    when (state_type /= getStateType proxy) mzero

    case A.eitherDecodeStrict (wxppTalkStateJson rec) of
        Left err -> do
            let err_msg = "cannot decode JSON ByteString: WxppTalkState #"
                                <> toPathPiece rec_id
                                <> ", " <> fromString err
            $logError err_msg
            throwError $ T.unpack err_msg

        Right jv -> do
            case A.parseEither parseJSON jv of
                Left jerr -> do
                    let err_msg = "cannot decode JSON Value: WxppTalkState #"
                                <> toPathPiece rec_id
                                <> ", " <> fromString jerr
                    $logError err_msg
                    throwError $ T.unpack err_msg

                Right x -> return x


saveSomeWxppTalkState :: forall m r.
    ( MonadIO m ) =>
    WxppTalkStateId
    -> SomeWxppTalkState r (ReaderT SqlBackend m)
    -> WxTalkerMonad r (ReaderT SqlBackend m) ()
saveSomeWxppTalkState = saveWxppTalkState getStateTypeOfSomeWxppTalkState


loadWxppTalkStateCurrent :: forall m.
    (MonadIO m) =>
    WxppOpenID
    -> ReaderT WxppDbBackend m (Maybe (Entity WxppTalkState))
loadWxppTalkStateCurrent open_id = do
    selectFirst [ WxppTalkStateOpenId ==. open_id ]
                [ Desc WxppTalkStateId ]


-- | used with loopRunBgJob
cleanUpTimedOutWxTalk :: MonadResource m =>
    (NominalDiffTime, NominalDiffTime)
    -> (WxppAppID -> WxppOpenID -> ReaderT SqlBackend m ())
    -> ReaderT SqlBackend m ()
cleanUpTimedOutWxTalk ttls on_abort_talk = do
    let timeout_ttl = uncurry min ttls
        chk_ttl     = uncurry max ttls
    now <- liftIO getCurrentTime
    let dt = addUTCTime (negate $ abs timeout_ttl) now

    -- 为保证下面的 SQL 不会从一个太大集合中查找
    let too_old = flip addUTCTime now $ negate $ chk_ttl

    infos <- selectSource
                [ WxppTalkStateDone       ==. False
                , WxppTalkStateAborted    ==. False
                , WxppTalkStateUpdatedTime <. dt
                , WxppTalkStateUpdatedTime >. too_old
                ]
                []
                $= ( CL.map $ \(Entity rec_id rec) ->
                                    (rec_id, (wxppTalkStateAppId &&& wxppTalkStateOpenId) rec)
                    )
                $$ CL.consume

    -- 一次性用一个 SQL 更新
    updateWhere
        [ WxppTalkStateId <-. map fst infos ]
        [ WxppTalkStateAborted =. True ]

    -- 然后通告用户
    forM_ infos $ \(rec_id, (app_id, open_id)) -> do
        m_new <- selectFirst
                     [ WxppTalkStateAppId     ==. app_id
                     , WxppTalkStateOpenId    ==. open_id
                     , WxppTalkStateId        >. rec_id
                     ]
                     []

        -- 仅当那个用户没有更新的会话已建立时才发通告給用户
        when (isNothing m_new) $ do
            on_abort_talk app_id open_id


-- | 作为下面 WxppTalkHandlerGeneral 的参数用
-- r0 是 WxppTalkHandlerGeneral 提供的全局环境
data WxppTalkerStateEntry r0 m = forall s r.
                            (Eq s, ToJSON s, FromJSON s, HasStateType s
                            , WxTalkerState (r0, r) (ReaderT WxppDbBackend m) s
                            , WxTalkerDoneAction (r0, r) (ReaderT WxppDbBackend m) s
                            ) =>
                            WxppTalkerStateEntry (Proxy s) r

-- | 这个通用的对话处理器
-- 所有输入都应经过这个处理器处理一次
-- 如果在对话中，则有相应的处理
-- 不在对话中，则相当于空操作
data WxppTalkHandlerGeneral r m = WxppTalkHandlerGeneral
  { wxppTalkDbRunner      :: WxppDbRunner -- ^ to run db functions
  , wxppTalkDbReadOnlyEnv :: r            -- ^ read only data/environment
  , wxppTalkDStateEntry   :: [WxppTalkerStateEntry r m]
  }

instance JsonConfigable (WxppTalkHandlerGeneral r m) where
    type JsonConfigableUnconfigData (WxppTalkHandlerGeneral r m) =
        (WxppDbRunner, r, [WxppTalkerStateEntry r m])

    isNameOfInMsgHandler _ x = x == "any-talk"

    parseWithExtraData _ (f1, f2, f3) _obj = return $ WxppTalkHandlerGeneral f1 f2 f3

type instance WxppInMsgProcessResult (WxppTalkHandlerGeneral r m) = WxppInMsgHandlerResult

instance (MonadBaseControl IO m, WxppApiMonad env m) =>
    IsWxppInMsgProcessor m (WxppTalkHandlerGeneral r m)
    where
    processInMsg (WxppTalkHandlerGeneral db_runner env entries) _cache _bs m_ime = runExceptT $ do
        case m_ime of
            Nothing -> return []
            Just ime -> do
                mapExceptT (runWxppDB db_runner) $ do
                    let open_id = wxppInFromUserName ime
                    m_state_rec <- lift $ loadWxppTalkStateCurrent open_id
                    case m_state_rec of
                        Nothing -> return []
                        Just e_state_rec@(Entity state_id _) -> do
                            let mk :: WxppTalkerStateEntry r m
                                    -> MaybeT (ExceptT String (ReaderT WxppDbBackend m)) WxppInMsgHandlerResult
                                mk (WxppTalkerStateEntry state_proxy rx) = MaybeT $ ExceptT $ processInMsgByWxTalk
                                                    state_proxy
                                                    (env, rx)
                                                    e_state_rec
                                                    ime
                            m_result <- runMaybeT $ asum $ map mk entries
                            case m_result of
                                Nothing -> do
                                    $logWarn $ "no handler could handle talk state: state_id="
                                                <> toPathPiece state_id
                                    return []
                                Just x -> return x

                        {-
                    m_state_info <- ExceptT $ loadWxppTalkStateCurrent open_id
                    case m_state_info of
                        Nothing -> return []
                        Just (db_id, _ :: SomeWxppTalkState CommonTalkEnv (ReaderT SqlBackend m) ) -> do
                            ExceptT $ processInMsgByWxTalk
                                        (mk_env $ wxppInFromUserName ime)
                                        db_id
                                        ime
                                        --}


-- | 消息处理器：调用后会新建一个会话
-- 对话状态由类型参数 s 指定
-- 因为它本身不带条件，所以常常配合条件判断器使用
-- 但也条件判断也可以在 wxTalkInitiate 里实现
data WxppTalkInitiator r s = WxppTalkInitiator
  { wxppTalkInitDbRunner   :: WxppDbRunner
  , wxppTalkInitEnv        :: r                           -- ^ 与对话种类无关的环境值
  , wxppTalkInitStateEnv   :: (WxppTalkStateExtraEnv s)     -- ^ 对话特定相关的环境值
  }

instance HasStateType s => JsonConfigable (WxppTalkInitiator r s) where
    type JsonConfigableUnconfigData (WxppTalkInitiator r s) =
            (WxppDbRunner, r, WxppTalkStateExtraEnv s)

    isNameOfInMsgHandler _ x =
        x == "initiate-talk:" <> getStateType (Proxy :: Proxy s)

    parseWithExtraData _ (f1, f2, f3) _obj = return $ WxppTalkInitiator f1 f2 f3


type instance WxppInMsgProcessResult (WxppTalkInitiator r s) = WxppInMsgHandlerResult

instance
    ( HasStateType s, ToJSON s, FromJSON s, Eq s
    , HasWxppAppID r
    , WxTalkerDoneAction (r, r2) (ReaderT WxppDbBackend m) s
    , WxTalkerState (r, r2) (ReaderT WxppDbBackend m) s
    , WxTalkerFreshState (r, r2) (ReaderT WxppDbBackend m) s
    , r2 ~ WxppTalkStateExtraEnv s
    , MonadBaseControl IO m, MonadIO m, MonadLogger m
    ) =>
    IsWxppInMsgProcessor m (WxppTalkInitiator r s)
    where
    processInMsg (WxppTalkInitiator db_runner env extra_env) _cache _bs m_ime = runExceptT $ do
        case m_ime of
            Nothing -> return []
            Just ime -> do
                    let from_open_id = wxppInFromUserName ime
                        app_id = getWxppAppID env
                    mapExceptT (runWxppDB db_runner) $ do
                        msgs_or_state <- flip runWxTalkerMonadE (env, extra_env) $ wxTalkInitiate ime
                        case msgs_or_state of
                            Left msgs -> do
                                        -- cannot create conversation
                                        return $ map ((False,) . Just) $ msgs

                            Right (state :: s) -> do
                                -- state_id <- lift $ newWxppTalkState' app_id from_open_id state
                                e_state <- lift $ newWxppTalkState' app_id from_open_id state
                                ExceptT $ processJustInitedWxTalk
                                            (Proxy :: Proxy s) (env, extra_env) e_state


-- | 与 WxppTalkerStateEntry 的区别只是多了 WxTalkerFreshState 的要求
data WxppTalkerFreshStateEntry r0 m = forall s r.
                                (Eq s, ToJSON s, FromJSON s, HasStateType s
                                , WxTalkerState (r0, r) (ReaderT WxppDbBackend m) s
                                , WxTalkerDoneAction (r0, r) (ReaderT WxppDbBackend m) s
                                , WxTalkerFreshState (r0, r) (ReaderT WxppDbBackend m) s
                                ) =>
                                WxppTalkerFreshStateEntry (Proxy s) r

wxppTalkerFreshStateEntryToStateEntry :: WxppTalkerFreshStateEntry r m -> WxppTalkerStateEntry r m
wxppTalkerFreshStateEntryToStateEntry (WxppTalkerFreshStateEntry p x) = WxppTalkerStateEntry p x

-- | 消息处理器：调用后会新建一个会话
-- 它接受的 event key 必须是以下的形式： initiate-talk:XXX
-- 其中 XXX 是某个对话状态的 getStateType 内容
-- 与 WxppTalkInitiator 类似，不同的是：
-- * WxppTalkInitiator 只能初始化确定的某种对话，
--   WxppTalkEvtKeyInitiator则在一组可能选择里选择一个
-- * WxppTalkInitiator 本身不带判断条件，
--   WxppTalkEvtKeyInitiator 则根据 event key 内容选择合适的对话类型
data WxppTalkEvtKeyInitiator r m = WxppTalkEvtKeyInitiator
  { wxppTalkEvtKeyInitDbRunner   :: WxppDbRunner
  , wxppTalkEvtKeyInitEventEnv   :: r                           -- ^ 与对话种类无关的环境值
  , wxppTalkEvtKeyInitStateEntry :: [WxppTalkerFreshStateEntry r m]
  }

instance JsonConfigable (WxppTalkEvtKeyInitiator r m) where
    type JsonConfigableUnconfigData (WxppTalkEvtKeyInitiator r m) =
            (WxppDbRunner, r, [WxppTalkerFreshStateEntry r m])

    isNameOfInMsgHandler _ x = x == "evtkey-initiate-talk"

    parseWithExtraData _ (f1, f2, f3) _obj = return $ WxppTalkEvtKeyInitiator f1 f2 f3


type instance WxppInMsgProcessResult (WxppTalkEvtKeyInitiator r m) = WxppInMsgHandlerResult

instance
    ( HasWxppAppID r
    , MonadBaseControl IO m, MonadIO m, MonadLogger m
    ) =>
    IsWxppInMsgProcessor m (WxppTalkEvtKeyInitiator r m)
    where
    processInMsg (WxppTalkEvtKeyInitiator db_runner env entries) _cache _bs m_ime = runExceptT $ do
        case m_ime of
            Nothing -> return []
            Just ime -> do
                case wxppInMessage ime of
                    WxppInMsgEvent (WxppEvtClickItem evtkey) -> do
                        case T.stripPrefix "initiate-talk:" evtkey of
                            Nothing     -> return []
                            Just st_type -> do_work ime st_type

                    _ -> return []

        where
            do_work ime st_type = do
                let match_st_type (WxppTalkerFreshStateEntry px _) = getStateType px == st_type
                case find match_st_type entries of
                    Nothing -> do
                        $logWarnS wxppLogSource $
                                "Failed to initiate talk from menu click,"
                                <> " because talk state type is unknown to me: "
                                <> st_type
                        return []

                    Just (WxppTalkerFreshStateEntry st_px extra_env) -> do
                        let from_open_id = wxppInFromUserName ime
                            app_id = getWxppAppID env
                        mapExceptT (runWxppDB db_runner) $ do
                            msgs_or_state <- flip runWxTalkerMonadE (env, extra_env) $
                                wxTalkInitiateBlank st_px from_open_id
                            case msgs_or_state of
                                Left msgs -> do
                                            -- cannot create conversation
                                            $logErrorS wxppLogSource $
                                                "Couldn't create talk, providing error output messages: " <> tshow msgs
                                            return $ map ((False,) . Just) $ msgs

                                Right state -> do
                                    -- state_id <- lift $ newWxppTalkState' app_id from_open_id state
                                    e_state <- lift $ newWxppTalkState' app_id from_open_id state
                                    ExceptT $ processJustInitedWxTalk st_px (env, extra_env) e_state


-- | 消息处理器：调用后会无条件结束当前会话
data WxppTalkTerminator = WxppTalkTerminator
  { wxppTalkTermAppId    :: WxppAppID
  , wxppTalkTermDir      :: (NonEmpty FilePath) -- ^ out-msg dir path
  , wxppTalkTermDbRunner :: WxppDbRunner
  , wxppTalkTermPrimary  :: Bool                -- ^ if primary
  , wxppTalkTermOurMsg   :: WxppOutMsgLoader    -- ^ 打算回复用户的消息
  }

instance JsonConfigable WxppTalkTerminator where
    type JsonConfigableUnconfigData WxppTalkTerminator = (WxppAppID, NonEmpty FilePath, WxppDbRunner)

    isNameOfInMsgHandler _ x = x == "terminate-talk"

    parseWithExtraData _ (f1, f2, f3) obj =
        WxppTalkTerminator f1 f2 f3
                <$> (obj .:? "primary" .!= True)
                <*> parseWxppOutMsgLoader obj


type instance WxppInMsgProcessResult WxppTalkTerminator = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadBaseControl IO m, MonadCatch m) =>
    IsWxppInMsgProcessor m WxppTalkTerminator where
    processInMsg (WxppTalkTerminator app_id msg_dirs db_runner primary get_outmsg) cache _bs m_ime = runExceptT $ do
        case m_ime of
            Nothing -> return []
            Just ime -> do
                    let from_open_id = wxppInFromUserName ime
                    mapExceptT (runWxppDB db_runner) $ do
                        lift $ abortCurrentWxppTalkState app_id from_open_id

                    let get_atk = (tryWxppWsResultE "getting access token" $ liftIO $
                                        wxppCacheGetAccessToken cache app_id)
                                    >>= maybe (throwError $ "no access token available") (return . fst)
                    outmsg <- ExceptT $ runDelayedYamlLoaderL msg_dirs get_outmsg
                    liftM (return . (primary,) . Just) $ tryWxppWsResultE "fromWxppOutMsgL" $
                                    tryYamlExcE $ fromWxppOutMsgL msg_dirs cache get_atk outmsg



processInMsgByWxTalk :: (HasStateType s, Eq s, FromJSON s, ToJSON s
                        , WxppApiMonad env m
                        , WxTalkerState r (ReaderT WxppDbBackend m) s
                        , WxTalkerDoneAction r (ReaderT WxppDbBackend m) s)  =>
                        Proxy s
                        -> r
                        -> Entity WxppTalkState
                        -> WxppInMsgEntity
                        -> ReaderT WxppDbBackend m (Either String (Maybe WxppInMsgHandlerResult))
processInMsgByWxTalk state_proxy env (Entity state_id state_rec) ime = do
    let state_type = wxppTalkStateTyp state_rec
    if (state_type /= getStateType state_proxy)
       then return $ Right Nothing
       else do
            -- 处理会话时，状态未必会更新
            -- 更新时间，以表明这个会话不是 idle 的
            now <- liftIO getCurrentTime
            update state_id [ WxppTalkStateUpdatedTime =. now ]

            liftM (fmap Just) $
                wxTalkerInputProcessInMsg
                    get_st set_st
                    env
                    (Just ime)
    where
        set_st _open_id = saveAnyWxppTalkState state_id
        get_st _open_id = mkWxTalkerMonad $ \_ -> loadAnyWxppTalkState state_proxy state_id


processJustInitedWxTalk :: (MonadIO m, MonadLogger m
                           , Eq s, FromJSON s, ToJSON s, HasStateType s
                           , WxTalkerDoneAction r (ReaderT WxppDbBackend m) s
                           , WxTalkerState r (ReaderT WxppDbBackend m) s
                           ) =>
                            Proxy s
                            -> r
                            -> WxppTalkStateId
                            -> ReaderT SqlBackend m (Either String WxppInMsgHandlerResult)
processJustInitedWxTalk state_proxy env state_id = runExceptT $ do
    ExceptT $ wxTalkerInputProcessJustInited get_st set_st env
    where
        set_st = saveAnyWxppTalkState state_id
        get_st = mkWxTalkerMonad $ \_ -> loadAnyWxppTalkState state_proxy state_id

