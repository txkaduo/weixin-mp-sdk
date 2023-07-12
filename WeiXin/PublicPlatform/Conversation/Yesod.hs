{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module WeiXin.PublicPlatform.Conversation.Yesod where

-- {{{1 imports
import ClassyPrelude
import Yesod
import Conduit
import Database.Persist.Sql
import qualified Control.Exception.Safe as ExcSafe
import Control.Monad.Logger
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

import WeiXin.PublicPlatform.Conversation
import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.Yesod.Model
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils
import WeiXin.PublicPlatform.Media
-- }}}1


saveWxppTalkState :: forall m a r. ( MonadLoggerIO m, ToJSON a, WxTalkerState r m a)
                  => WxppDbRunner
                  -> (a -> Text)
                  -> WxppTalkStateId
                  -> a
                  -> WxTalkerMonad r m ()
saveWxppTalkState db_runner get_state_type state_id x = mkWxTalkerMonad $ \env -> runExceptT $ do
    now <- liftIO getCurrentTime
    done <- ExceptT $ flip runWxTalkerMonad env $ wxTalkIfDone x
    log_func <- askLoggerIO
    liftIO $ flip runLoggingT log_func $ runWxppDB db_runner $ do
      update state_id
            [ WxppTalkStateTyp          =. get_state_type x
            , WxppTalkStateJson         =. (LB.toStrict $ A.encode x)
            , WxppTalkStateDone         =. done
            , WxppTalkStateUpdatedTime  =. now
            ]


abortCurrentWxppTalkState :: forall m r.  (MonadLoggerIO m)
                          => WxppDbRunner
                          -> r
                          -> (Text -> Maybe (WxppTalkerAbortStateEntry r m))
                          -- ^ lookup WxppTalkerAbortStateEntry by state's type string
                          -> WxTalkAbortInitiator
                          -> WxppAppID
                          -> WxppOpenID
                          -> m (Maybe [WxppOutMsg])
-- {{{1
abortCurrentWxppTalkState db_runner common_env lookup_se initiator app_id open_id = do
  log_func <- askLoggerIO
  m_rec <- liftIO $ flip runLoggingT log_func $ runWxppDB db_runner $ loadWxppTalkStateCurrent app_id open_id

  fmap join $ forM m_rec $ \ e_rec@(Entity rec_id rec) -> do
    if not $ wxppTalkStateAborted rec || wxppTalkStateDone rec
       then fmap Just $ do
            out_msgs <- wxppExecTalkAbortForRecord common_env lookup_se initiator e_rec
            liftIO $ flip runLoggingT log_func $ runWxppDB db_runner $ update rec_id [ WxppTalkStateAborted =. True ]
            return out_msgs

       else return Nothing
-- }}}1


wxppExecTalkAbortForRecord :: (MonadLogger m)
                           => r
                           -> (Text -> Maybe (WxppTalkerAbortStateEntry r m))
                           -- ^ lookup WxppTalkerAbortStateEntry by state's type string
                           -> WxTalkAbortInitiator
                           -> Entity WxppTalkState
                           -> m [WxppOutMsg]
wxppExecTalkAbortForRecord common_env lookup_se initiator e_rec@(Entity rec_id rec) = do
-- {{{1
  case lookup_se typ_str of
    Nothing -> do
      $logWarnS wxppLogSource $ "could not find state entry for record #" <> toPathPiece rec_id
                  <> ", type string was: " <> typ_str
      return []

    Just (WxppTalkerAbortStateEntry sp ext_env) -> do
      err_or_st <- parseWxppTalkStateFromRecord sp e_rec
      case err_or_st of
        Left err -> do
          $logErrorS wxppLogSource $ "parseWxppTalkStateFromRecord failed for record #" <> toPathPiece rec_id
                        <> ": " <> fromString err
          return []

        Right Nothing -> return []

        Right (Just st) -> do
          err_or_outmsgs <- flip runWxTalkerMonad (common_env, ext_env) $ wxTalkAbort st initiator
          case err_or_outmsgs of
            Left err -> do
              $logErrorS wxppLogSource $ "wxTalkAbort failed for record #" <> toPathPiece rec_id
                          <> ": " <> fromString err
              return []

            Right x -> return x

  where typ_str = wxppTalkStateTyp rec
-- }}}1


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


saveAnyWxppTalkState :: forall m r a. (MonadLoggerIO m, ToJSON a, HasStateType a, WxTalkerState r m a)
                     => WxppDbRunner
                     -> WxppTalkStateId
                     -> a
                     -> WxTalkerMonad r m ()
saveAnyWxppTalkState db_runner = saveWxppTalkState db_runner getStateType'

loadAnyWxppTalkState :: forall m a.  (MonadLoggerIO m, FromJSON a, HasStateType a)
                     => WxppDbRunner
                     -> Proxy a
                     -> WxppTalkStateId
                     -> m (Either String (Maybe a))
loadAnyWxppTalkState db_runner proxy state_id = runExceptT $ runMaybeT $ do
  log_func <- askLoggerIO
  rec <- MaybeT $ liftIO $ flip runLoggingT log_func $ runWxppDB db_runner $ get state_id
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
            $logErrorS wxppLogSource err_msg
            throwError $ T.unpack err_msg

        Right jv -> do
            case A.parseEither parseJSON jv of
                Left jerr -> do
                    let err_msg = "cannot decode JSON Value: WxppTalkState #"
                                <> toPathPiece rec_id
                                <> ", " <> fromString jerr
                    $logErrorS wxppLogSource err_msg
                    throwError $ T.unpack err_msg

                Right x -> return x


{-
saveSomeWxppTalkState :: forall m r.
    ( MonadIO m ) =>
    WxppTalkStateId
    -> SomeWxppTalkState r (ReaderT SqlBackend m)
    -> WxTalkerMonad r (ReaderT SqlBackend m) ()
saveSomeWxppTalkState = saveWxppTalkState getStateTypeOfSomeWxppTalkState
--}


loadWxppTalkStateCurrent :: forall m.  (MonadIO m)
                          => WxppAppID
                          -> WxppOpenID
                          -> ReaderT WxppDbBackend m (Maybe (Entity WxppTalkState))
loadWxppTalkStateCurrent app_id open_id = do
    selectFirst [ WxppTalkStateOpenId ==. open_id
                , WxppTalkStateAppId ==. app_id
                ]
                [ Desc WxppTalkStateId ]


-- | used with loopRunBgJob
cleanUpTimedOutWxTalk :: (MonadLoggerIO m, HasWxppAppID r)
                      => WxppDbRunner
                      -> r
                      -> [WxppTalkerAbortStateEntry r m]
                      -- ^ lookup WxppTalkerAbortStateEntry by state's type string
                      -> (NominalDiffTime, NominalDiffTime)
                      -> (WxppAppID -> WxppOpenID -> [WxppOutMsg] -> m ())
                      -> m ()
cleanUpTimedOutWxTalk db_runner common_env entries ttls on_abort_talk = do
-- {{{1
    let timeout_ttl = uncurry min ttls
        chk_ttl     = uncurry max ttls
    now <- liftIO getCurrentTime
    let dt = addUTCTime (negate $ abs timeout_ttl) now

    -- 为保证下面的 SQL 不会从一个太大集合中查找
    let too_old = flip addUTCTime now $ negate $ chk_ttl

    log_func <- askLoggerIO
    m_new_records <- liftIO $ runResourceT $ flip runLoggingT log_func $ runWxppDB db_runner $ do
      infos <- runConduit $
                selectSource
                  [ WxppTalkStateDone       ==. False
                  , WxppTalkStateAborted    ==. False
                  , WxppTalkStateAppId      ==. app_id
                  , WxppTalkStateUpdatedTime <. dt
                  , WxppTalkStateUpdatedTime >. too_old
                  ]
                  []
                  .| CL.map (id &&& (wxppTalkStateOpenId . entityVal))
                  .| CL.consume

      -- 一次性用一个 SQL 更新
      updateWhere
          [ WxppTalkStateId <-. map (entityKey . fst) infos ]
          [ WxppTalkStateAborted =. True ]

      -- 然后通告用户
      forM infos $ \(e_rec@(Entity rec_id _), open_id) -> do
          m_new <- selectFirst
                       [ WxppTalkStateAppId     ==. app_id
                       , WxppTalkStateOpenId    ==. open_id
                       , WxppTalkStateId        >. rec_id
                       ]
                       []
          return (e_rec, open_id, m_new)

    forM_ m_new_records $ \ (e_rec, open_id, m_new) -> do
      -- 仅当那个用户没有更新的会话已建立时才发通告給用户
      when (isNothing m_new) $ do
        out_msgs <- wxppExecTalkAbortForRecord common_env lookup_se WxTalkAbortBySys e_rec
        on_abort_talk app_id open_id out_msgs

    where
        app_id = getWxppAppID common_env
        match_entry typ_str (WxppTalkerAbortStateEntry p _) = getStateType p == typ_str
        lookup_se = \ x -> find (match_entry x) entries
-- }}}1


-- | 仅是为了减少代码重复而设
-- 由于这个对象可以构造其它需求稍低一些的 WxppTalkerStateEntry WxppTalkerFreshStateEntry WxppTalkerAbortStateEntry
data WxppTalkerFullStateEntry r0 m = forall s r.
  (Eq s, ToJSON s, FromJSON s, HasStateType s
  , WxTalkerState (r0, r) m s
  , WxTalkerDoneAction (r0, r) m s
  , WxTalkerAbortAction (r0, r) m s
  , WxTalkerFreshState (r0, r) m s
  )
  => WxppTalkerFullStateEntry (Proxy s) r


-- | 作为下面 WxppTalkHandlerGeneral 的参数用
-- r0 是 WxppTalkHandlerGeneral 提供的全局环境
data WxppTalkerStateEntry r0 m = forall s r.
                            (Eq s, ToJSON s, FromJSON s, HasStateType s
                            , WxTalkerState (r0, r) m s
                            , WxTalkerDoneAction (r0, r) m s
                            ) =>
                            WxppTalkerStateEntry (Proxy s) r


wxppTalkerStateEntryFromFull :: WxppTalkerFullStateEntry r m -> WxppTalkerStateEntry r m
wxppTalkerStateEntryFromFull (WxppTalkerFullStateEntry p x) = WxppTalkerStateEntry p x


-- | 这个通用的对话处理器
-- 所有输入都应经过这个处理器处理一次
-- 如果在对话中，则有相应的处理
-- 不在对话中，则相当于空操作
data WxppTalkHandlerGeneral r m = WxppTalkHandlerGeneral
  { wxppTalkDbRunner      :: WxppDbRunner -- ^ to run db functions
  , wxppTalkDbReadOnlyEnv :: r            -- ^ read only data/environment
  , wxppTalkDStateEntry   :: [WxppTalkerStateEntry r m ]
  }

instance JsonConfigable (WxppTalkHandlerGeneral r m) where
    type JsonConfigableUnconfigData (WxppTalkHandlerGeneral r m) =
        (WxppDbRunner, r, [WxppTalkerStateEntry r m])

    isNameOfInMsgHandler _ x = x == "any-talk"

    parseWithExtraData _ (f1, f2, f3) _obj = return $ WxppTalkHandlerGeneral f1 f2 f3

type instance WxppInMsgProcessResult (WxppTalkHandlerGeneral r m) = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadLoggerIO m) =>
    IsWxppInMsgProcessor m (WxppTalkHandlerGeneral r m)
    where
    processInMsg (WxppTalkHandlerGeneral db_runner env entries) _cache app_info _bs ime = do
      log_func <- askLoggerIO
      runExceptT $ do
        m_state_rec <- mapExceptT (liftIO . flip runLoggingT log_func . runWxppDB db_runner) $ do
            lift $ loadWxppTalkStateCurrent app_id open_id

        case m_state_rec of
            Nothing -> return []
            Just e_state_rec@(Entity state_id _) -> do
                let mk :: WxppTalkerStateEntry r m -> MaybeT (ExceptT String m) WxppInMsgHandlerResult
                    mk (WxppTalkerStateEntry state_proxy rx) = MaybeT $ ExceptT $
                                    processInMsgByWxTalk
                                        db_runner
                                        state_proxy
                                        (env, rx)
                                        e_state_rec
                                        ime
                m_result <- runMaybeT $ asum $ map mk entries
                case m_result of
                    Nothing -> do
                        $logWarnS wxppLogSource $ "no handler could handle talk state: state_id="
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

      where
        app_id = procAppIdInfoReceiverId app_info
        open_id = wxppInFromUserName ime


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
    , WxTalkerDoneAction (r, r2) m s
    , WxTalkerState (r, r2) m s
    , WxTalkerFreshState (r, r2) m s
    , r2 ~ WxppTalkStateExtraEnv s
    , MonadLoggerIO m
    ) =>
    IsWxppInMsgProcessor m (WxppTalkInitiator r s)
    where
    processInMsg (WxppTalkInitiator db_runner env extra_env) _cache _app_info _bs ime =
      runExceptT $ do
        let from_open_id = wxppInFromUserName ime
            app_id = getWxppAppID env

        log_func <- askLoggerIO
        msgs_or_state <- flip runWxTalkerMonadE (env, extra_env) $ wxTalkInitiate ime

        case msgs_or_state of
            Left msgs -> do
                        -- cannot create conversation
                        return $ map ((False,) . Just) $ msgs

            Right (state :: s) -> do
                -- state_id <- lift $ newWxppTalkState' app_id from_open_id state
                e_state <- liftIO . flip runLoggingT log_func . runWxppDB db_runner $ do
                            newWxppTalkState' app_id from_open_id state
                ExceptT $ processJustInitedWxTalk db_runner
                            (Proxy :: Proxy s) (env, extra_env) e_state


-- | 与 WxppTalkerStateEntry 的区别只是多了 WxTalkerFreshState 的要求
data WxppTalkerFreshStateEntry r0 m = forall s r.
                                (Eq s, ToJSON s, FromJSON s, HasStateType s
                                , WxTalkerState (r0, r) m s
                                , WxTalkerDoneAction (r0, r) m s
                                , WxTalkerFreshState (r0, r) m s
                                ) =>
                                WxppTalkerFreshStateEntry (Proxy s) r

wxppTalkerFreshStateEntryFromFull :: WxppTalkerFullStateEntry r m -> WxppTalkerFreshStateEntry r m
wxppTalkerFreshStateEntryFromFull (WxppTalkerFullStateEntry p x) = WxppTalkerFreshStateEntry p x

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
    , MonadLoggerIO m
    ) =>
    IsWxppInMsgProcessor m (WxppTalkEvtKeyInitiator r m)
    where
    processInMsg (WxppTalkEvtKeyInitiator db_runner env entries) _cache _app_info _bs ime =
      runExceptT $ do
        case wxppInMessage ime of
            WxppInMsgEvent (WxppEvtClickItem evtkey) -> do
                case T.stripPrefix "initiate-talk:" evtkey of
                    Nothing     -> return []
                    Just st_type -> do_work st_type

            _ -> return []

        where
            do_work st_type = do
                let match_st_type (WxppTalkerFreshStateEntry px _) = getStateType px == st_type
                log_func <- askLoggerIO
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
                                e_state <- liftIO $ flip runLoggingT log_func $ runWxppDB db_runner $
                                  newWxppTalkState' app_id from_open_id state
                                ExceptT $ processJustInitedWxTalk db_runner st_px (env, extra_env) e_state


-- | 与 WxppTalkerStateEntry 的区别只是多了 WxTalkerFreshState 的要求
data WxppTalkerAbortStateEntry r0 m = forall s r.
  (Eq s, ToJSON s, FromJSON s, HasStateType s
  , WxTalkerAbortAction (r0, r) m s
  )
  => WxppTalkerAbortStateEntry (Proxy s) r

wxppTalkerAbortStateEntryFromFull :: WxppTalkerFullStateEntry r m -> WxppTalkerAbortStateEntry r m
wxppTalkerAbortStateEntryFromFull (WxppTalkerFullStateEntry p x) = WxppTalkerAbortStateEntry p x


-- | 消息处理器：调用后会无条件结束当前会话
data WxppTalkTerminator r m = WxppTalkTerminator
  { wxppTalkTermDir          :: (NonEmpty FilePath) -- ^ out-msg dir path
  , wxppTalkTermDbRunner     :: WxppDbRunner
  , wxppTalkTermCommonEnv    :: r            -- ^ read only data/environment
  , wxppTalkTermStateEntries :: [ WxppTalkerAbortStateEntry r m ]
  -- , wxppTalkTermPrimary  :: Bool                -- ^ if primary
  -- 一但这个处理器被调用，则很可能已发生数据库的实质修改
  -- 这里再指定是否 primary 已无太大意义，应总是理解为primary响应
  , wxppTalkTermOurMsg       :: WxppOutMsgLoader    -- ^ 打算回复用户的消息
  }

instance JsonConfigable (WxppTalkTerminator r m) where
  type JsonConfigableUnconfigData (WxppTalkTerminator r m) = (NonEmpty FilePath, WxppDbRunner, r, [ WxppTalkerAbortStateEntry r m ])

  isNameOfInMsgHandler _ x = x == "terminate-talk"

  parseWithExtraData _ (f1, f2, f3, f4) obj =
    WxppTalkTerminator f1 f2 f3 f4
            -- <$> (obj .:? "primary" .!= True)
            <$> parseWxppOutMsgLoader obj


type instance WxppInMsgProcessResult (WxppTalkTerminator r m) = WxppInMsgHandlerResult

instance (WxppApiMonad env m, MonadLoggerIO m, ExcSafe.MonadCatch m) =>
  IsWxppInMsgProcessor m (WxppTalkTerminator r m) where
    processInMsg (WxppTalkTerminator msg_dirs db_runner common_env entries get_outmsg) cache app_info _bs ime =
-- {{{1
      runExceptT $ do
        let from_open_id = wxppInFromUserName ime

        m_out_msgs_abort <-
          lift $ abortCurrentWxppTalkState db_runner common_env (\ x -> find (match_entry x) entries) WxTalkAbortByUser app_id from_open_id

        liftM (fromMaybe []) $ forM m_out_msgs_abort $ \ out_msgs_abort -> do
          let get_atk = (tryWxppWsResultE "getting access token" $ liftIO $
                              wxppCacheGetAccessToken cache app_id)
                          >>= maybe (throwError $ "no access token available") (return . fst)
          outmsg_l <- ExceptT $ runDelayedYamlLoaderL msg_dirs get_outmsg
          out_msg <- tryWxppWsResultE "fromWxppOutMsgL" $
                          tryYamlExcE $ fromWxppOutMsgL msg_dirs cache get_atk outmsg_l
          return $ map ((primary,) . Just) $ out_msgs_abort <> [ out_msg ]
      where
        primary = True
        app_id = procAppIdInfoReceiverId app_info
        match_entry typ_str (WxppTalkerAbortStateEntry p _) = getStateType p == typ_str
-- }}}1



processInMsgByWxTalk :: (HasStateType s, Eq s, FromJSON s, ToJSON s
                        , MonadLoggerIO m
                        -- , WxppApiMonad env m
                        , WxTalkerState r m s
                        , WxTalkerDoneAction r m s
                        )
                     => WxppDbRunner
                     -> Proxy s
                     -> r
                     -> Entity WxppTalkState
                     -> WxppInMsgEntity
                     -> m (Either String (Maybe WxppInMsgHandlerResult))
processInMsgByWxTalk db_runner state_proxy env (Entity state_id state_rec) ime = do
    let state_type = wxppTalkStateTyp state_rec
    if (state_type /= getStateType state_proxy)
       then return $ Right Nothing
       else do
            -- 处理会话时，状态未必会更新
            -- 更新时间，以表明这个会话不是 idle 的
            now <- liftIO getCurrentTime
            log_func <- askLoggerIO
            liftIO $ flip runLoggingT log_func $ runWxppDB db_runner $
              update state_id [ WxppTalkStateUpdatedTime =. now ]

            liftM (fmap Just) $
                wxTalkerInputProcessInMsg
                    get_st set_st
                    env
                    (Just ime)
    where
        set_st _open_id = saveAnyWxppTalkState db_runner state_id
        get_st _open_id = mkWxTalkerMonad $ \_ -> loadAnyWxppTalkState db_runner state_proxy state_id


processJustInitedWxTalk :: ( MonadLoggerIO m
                           , Eq s, FromJSON s, ToJSON s, HasStateType s
                           , WxTalkerDoneAction r m s
                           , WxTalkerState r m s
                           )
                        => WxppDbRunner
                        -> Proxy s
                        -> r
                        -> WxppTalkStateId
                        -> m (Either String WxppInMsgHandlerResult)
processJustInitedWxTalk db_runner state_proxy env state_id = runExceptT $ do
  ExceptT $ wxTalkerInputProcessJustInited get_st set_st env
  where set_st = saveAnyWxppTalkState db_runner state_id
        get_st = mkWxTalkerMonad $ \_ -> loadAnyWxppTalkState db_runner state_proxy state_id



-- vim: set foldmethod=marker:
