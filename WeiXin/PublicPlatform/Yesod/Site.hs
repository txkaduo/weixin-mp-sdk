{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module WeiXin.PublicPlatform.Yesod.Site
    ( module WeiXin.PublicPlatform.Yesod.Site
    , module WeiXin.PublicPlatform.Yesod.Site.Data
    ) where

-- {{{1
import ClassyPrelude
import Yesod
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Base64.URL as B64L
import qualified Data.ByteString.Char8      as C8
import qualified Data.Text                  as T
import qualified Data.Set                   as Set
import Control.Monad.Except                 (runExceptT, ExceptT(..), throwError, catchError)
import Control.Monad.Trans.Maybe            (runMaybeT, MaybeT(..))
#if !MIN_VERSION_classy_prelude(1, 0, 0)
import Control.Concurrent.Async             (async)
#endif
import Control.Concurrent                   (forkIO)
import Network.URI                          ( parseURI, uriQuery, uriToString )
import Network.HTTP                         ( urlEncode )
import Yesod.Default.Util                   ( widgetFileReload )
import Data.Time                            ( addUTCTime )
import System.Random                        (randomIO)

import Yesod.Helpers.Handler                ( httpErrorRetryWithValidParams
                                            , reqPathPieceParamPostGet
                                            , getCurrentUrl
                                            )
import Yesod.Helpers.Types                  (B64UByteStringPathPiece(..))
import Yesod.Helpers.Logger
import Yesod.Helpers.Utils                  (urlUpdateQueryText)
import Control.Monad.Logger

import Network.Wai                          (lazyRequestBody)
import Text.XML                             (renderText, parseLBS)
import Text.XML.Cursor                      (fromDocument)
import Data.Default                         (def)
import qualified Data.Text.Lazy             as LT
import Yesod.Core.Types                     (HandlerContents(HCError))
import Data.Yaml                            (decodeEither')
import qualified Data.List.NonEmpty         as LNE
import Network.HTTP.Types.Status            (mkStatus)
import Data.Conduit
import Data.Conduit.Binary                  (sinkLbs)
import qualified Data.Conduit.List          as CL
import qualified Data.Conduit.Combinators   as CC
import qualified Data.Aeson                 as A

import Database.Persist.Sql

import WeiXin.PublicPlatform.Yesod.Types    (handlerGetWeixinClientVersion)
import WeiXin.PublicPlatform.Yesod.Model
import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Class
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Message
import WeiXin.PublicPlatform.Error
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.EndUser
import WeiXin.PublicPlatform.QRCode
import WeiXin.PublicPlatform.OAuth
import WeiXin.PublicPlatform.ThirdParty
import WeiXin.PublicPlatform.Utils
-- }}}1


withWxppSubHandler :: ( MonadHandler m, HandlerSite m ~ MaybeWxppSub)
                   => (WxppSub -> m a)
                   -> m a
withWxppSubHandler f = do
    getYesod
        >>= liftIO . unMaybeWxppSub
        >>= maybe notFound return
        >>= f

checkSignature :: (HasWxppToken a, RenderMessage site FormMessage)
               => a
               -> Text  -- ^ GET param name of signature
               -> Text  -- ^ signed message text
               -> HandlerT site (HandlerT master IO) ()
-- {{{1
checkSignature foundation sign_param msg = do

    let token = getWxppToken foundation

        check_sign (tt, nn, sign) =
            if B16.encode sign0 == encodeUtf8 ( T.toLower sign )
                then Right ()
                else Left $ "invalid signature"
            where
                sign0 = wxppSignature token tt nn msg

    (tt, nn, sign) <- runInputGet $
                        (,,) <$> (TimeStampS <$> ireq textField "timestamp")
                             <*> (Nonce <$> ireq textField "nonce")
                             <*> ireq textField sign_param

    case check_sign (tt, nn, sign) of
      Left err  -> do $logErrorS wxppLogSource $ "got invalid signature: " <> sign_param
                      invalidArgs $ return err

      Right _   -> return ()
-- }}}1


-- | 用于修改 HandlerT 里日志的实现
withWxppSubLogging :: LoggingTRunner r
                   => r
                   -> HandlerT site (HandlerT master m) a
                   -> HandlerT site (HandlerT master m) a
withWxppSubLogging foundation h = do
    runLoggingTWith foundation $ LoggingT $ \log_func -> do
        withLogFuncInHandlerT log_func h


getMessageR :: HandlerT MaybeWxppSub (HandlerT master IO) Text
getMessageR = withWxppSubHandler $ \foundation -> do
    withWxppSubLogging foundation $ do
        checkSignature foundation "signature" ""
        runInputGet $ ireq textField "echostr"

postMessageR :: HandlerT MaybeWxppSub (HandlerT master IO) Text
postMessageR = withWxppSubHandler $ \foundation -> withWxppSubLogging foundation $ do
  let app_id = getWxppAppID foundation
  realHandlerMsg foundation (ProcAppSingle app_id)

realHandlerMsg :: forall site master a.
               (RenderMessage site FormMessage, HasWxppToken a,
               HasAesKeys a, HasWxppProcessor a)
               => a
               -> ProcAppIdInfo
               -> HandlerT site (HandlerT master IO) Text
-- {{{1
realHandlerMsg foundation app_info = do
    checkSignature foundation "signature" ""
    m_enc_type <- lookupGetParam "encrypt_type"
    enc <- case m_enc_type of
            Nothing -> return False
            Just "" -> return False
            Just "aes" -> return True
            Just x -> do
                        $(logErrorS) wxppLogSource $
                            "unknown/unsupported encrypt_type: " <> x
                        httpErrorRetryWithValidParams $ T.pack $
                            "Retry with valid parameters: encrypt_type(not supported)"
    req <- waiRequest
    lbs <- liftIO $ lazyRequestBody req
    let aks         = getAesKeys foundation
        app_token   = getWxppToken foundation
        processor   = getWxppProcessor foundation

    err_or_resp <- runExceptT $ do
        (decrypted_xml0, m_enc_akey) <-
            if enc
                then do
                    (either throwError return $ parse_xml_lbs lbs >>= wxppTryDecryptByteStringDocumentE my_app_id aks)
                        >>= maybe (throwError $ "Internal Error: no AesKey available to decrypt")
                                (return . (LB.fromStrict *** Just))
                else return (lbs, Nothing)

        let err_or_parsed = parse_xml_lbs decrypted_xml0 >>= wxppInMsgEntityFromDocument
        m_ime0 <- case err_or_parsed of
                    Left err -> do
                        $logErrorS wxppLogSource $ fromString $ "Error when parsing incoming XML: " ++ err
                        return Nothing
                    Right x -> return $ Just x

        ime0 <- case m_ime0 of
                  Nothing -> do
                    -- cannot parse incoming XML message
                    liftIO $ wxppOnParseInMsgError processor app_info lbs
                    throwError $ "Cannot parse incoming XML"

                  Just x -> return x

        pre_result <- liftIO $ wxppPreProcessInMsg processor app_info decrypted_xml0 ime0

        case pre_result of
            Left err -> do
                $logErrorS wxppLogSource $ "wxppPreProcessInMsg failed: " <> fromString err
                throwError "程序内部错误，请稍后重试"

            Right Nothing -> do
                $logDebugS wxppLogSource $ "message handle skipped because middleware return Nothing"
                return ("", Nothing)

            Right (Just (decrypted_xml, ime)) -> do
                let user_open_id    = wxppInFromUserName ime
                    target_username = wxppInToUserName ime

                let handle_msg      = wxppMsgHandler processor
                    try_handle_msg = ExceptT $ do
                                tryAny (liftIO $ handle_msg app_info decrypted_xml ime)
                                    >>= \err_or_x -> do
                                            case err_or_x of
                                              Left err -> do
                                                $logErrorS wxppLogSource $
                                                    "error when handling incoming message: " <> tshow err
                                                return $ Left $ show err
                                              Right x -> return x

                let post_handle_msg = wxppPostProcessInMsg processor
                    do_post_handle_msg out_res0 = ExceptT $ do
                        tryAny (liftIO $ post_handle_msg app_info decrypted_xml ime out_res0)
                            >>= \err_or_x -> do
                                    case err_or_x of
                                      Left err -> do
                                        $logErrorS wxppLogSource $
                                            "error when post-handling incoming message: " <> tshow err
                                        return $ Left $ show err
                                      Right x -> return x

                let do_on_error err = ExceptT $
                        liftIO (wxppOnProcessInMsgError processor app_info decrypted_xml ime err)

                out_res <- (try_handle_msg `catchError` \err -> do_on_error err >> throwError err)
                                >>= do_post_handle_msg

                let (primary_out_msgs, secondary_out_msgs) = (map snd *** map snd) $ partition fst out_res

                -- 只要有 primary 的回应，就忽略非primary的回应
                -- 如果没 primary 回应，而 secondary 回应有多个，则只选择第一个
                let split_head ls = case ls of
                                    [] -> (Nothing, [])
                                    (x:xs) -> (Just x, xs)
                let (m_resp_out_msg, other_out_msgs) =
                        if null primary_out_msgs
                            then (, []) $ listToMaybe $ catMaybes secondary_out_msgs
                            else split_head $ catMaybes primary_out_msgs

                now <- liftIO getCurrentTime
                let mk_out_msg_entity x = WxppOutMsgEntity
                                            user_open_id
                                            target_username
                                            now
                                            x

                let extra_data = map (user_open_id,) other_out_msgs

                liftM (, Just extra_data) $
                    fmap (fromMaybe "") $ forM m_resp_out_msg $ \out_msg -> do
                        liftM (LT.toStrict . renderText def) $ do
                            let out_msg_entity = mk_out_msg_entity out_msg
                            case m_enc_akey of
                                Just enc_akey ->
                                    ExceptT $ wxppOutMsgEntityToDocumentE
                                                    my_app_id app_token enc_akey out_msg_entity
                                Nothing ->
                                    return $ wxppOutMsgEntityToDocument out_msg_entity

    case err_or_resp of
        Left err -> do
            $(logErrorS) wxppLogSource $ fromString $
                "cannot encode outgoing message into XML: " <> err
            throwM $ HCError $
                InternalError "cannot encode outgoing message into XML"

        Right (xmls, m_extra_data) -> do
          forM_ m_extra_data $ \other_out_msgs -> do
            when (not $ null other_out_msgs) $ do
                void $ liftIO $ async $ do
                    -- 延迟半秒只要为了让直接回复的回应能第一个到达用户
                    threadDelay $ 1000 * 500
                    wxppSendOutMsgs processor to_app_id other_out_msgs

          return xmls

    where
        parse_xml_lbs x  = case parseLBS def x of
                                Left ex     -> Left $ "Failed to parse XML: " <> show ex
                                Right xdoc  -> return xdoc

        to_app_id = procAppIdInfoReceiverId app_info
        my_app_id = procAppIdInfoMyId app_info
-- }}}1


-- | 生成随机字串作为 oauth 的state参数之用
-- 这是按官方文档的思路，用于防 csrf
-- 所生成的随机字串会放在 sessionKeyWxppOAuthState 会话变量里
wxppOAuthMakeRandomState :: (MonadHandler m)
                         => WxppAppID
                         -> m Text
-- {{{1
wxppOAuthMakeRandomState app_id = do
    m_oauth_random_st <- lookupSession (sessionKeyWxppOAuthState app_id)
    case m_oauth_random_st of
        Just x | not (null x) -> do
            return x
        _   -> do
            random_state <- liftIO $ fmap (toPathPiece . B64UByteStringPathPiece) $
                                fmap B.pack $ replicateM 8 randomIO
            setSession (sessionKeyWxppOAuthState app_id) random_state
            return random_state
-- }}}1


wxppOAuthLoginRedirectUrl :: (MonadHandler m)
                          => (Route MaybeWxppSub -> [(Text, Text)] -> m Text)
                          -> Maybe WxppAppID
                          -- ^ 如果我方是第三方平台中服务方，需指定此参数
                          -> WxppAppID
                          -> OAuthScope
                          -> Text             -- ^ oauth's state param
                          -> UrlText          -- ^ return URL
                          -> m UrlText
-- {{{1
wxppOAuthLoginRedirectUrl url_render m_comp_app_id app_id scope user_st return_url = do
    random_state <- wxppOAuthMakeRandomState app_id
    let state = random_state <> ":" <> user_st

    oauth_retrurn_url <- liftM UrlText $
                            url_render OAuthCallbackR [ ("return", unUrlText return_url) ]

    let auth_url = wxppOAuthRequestAuthInsideWx m_comp_app_id app_id scope
                        oauth_retrurn_url
                        state
    return auth_url
-- }}}1


sessionKeyWxppUser :: WxppAppID -> Text
sessionKeyWxppUser app_id = "wx|" <> unWxppAppID app_id

sessionKeyWxppOAuthState :: WxppAppID -> Text
sessionKeyWxppOAuthState app_id = "wx-oauth-st|" <> unWxppAppID app_id

sessionKeyWxppUnionId :: Text
sessionKeyWxppUnionId = "wx-union-id"

sessionMarkWxppUser :: MonadHandler m
                    => WxppAppID
                    -> WxppOpenID
                    -> Maybe WxppUnionID
                    -> m ()
-- {{{1
sessionMarkWxppUser app_id open_id m_union_id = do
    setSession (sessionKeyWxppUser app_id) (unWxppOpenID open_id)
    -- XXX: union id 目前设在一个指定的键下面。
    -- 下面的做法不能支持多个系统的union id
    case fmap unWxppUnionID m_union_id of
        Just union_id | not (null union_id) -> setSession sessionKeyWxppUnionId union_id
        _                                   -> deleteSession sessionKeyWxppUnionId
-- }}}1


-- | 从 session 里找已登录的 WxppOpenID
sessionGetWxppUser :: MonadHandler m
                    => WxppAppID
                    -> m (Maybe WxppOpenID)
sessionGetWxppUser app_id = fmap (fmap WxppOpenID) $ lookupSession (sessionKeyWxppUser app_id)


-- | 从 session 里找已登录的 WxppOpenID 及 WxppUnionID
sessionGetWxppUserU :: MonadHandler m => WxppAppID -> m (Maybe (WxppAppOpenID, Maybe WxppUnionID))
sessionGetWxppUserU app_id = runMaybeT $ do
  open_id <- MaybeT $ sessionGetWxppUser app_id
  m_union_id <- lift $ fmap WxppUnionID <$> lookupSession sessionKeyWxppUnionId
  return (WxppAppOpenID app_id open_id, m_union_id)


getOAuthCallbackR :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) Html
-- {{{1
getOAuthCallbackR = withWxppSubHandler $ \sub -> do
    m_code <- lookupGetParam "code"
    return_url <- reqPathPieceParamPostGet "return"
    let app_id = getWxppAppID sub
        secret = getWxppSecret sub
        cache  = wxppSubCacheBackend sub

    oauth_state <- liftM (fromMaybe "") $ lookupGetParam "state"
    let (expected_st, state') = T.breakOn ":" oauth_state
    m_expected_state <- lookupSession (sessionKeyWxppOAuthState app_id)
    unless (m_expected_state == Just expected_st) $ do
        $logErrorS wxppLogSource $
            "OAuth state check failed, got: " <> oauth_state
        throwM $ HCError NotAuthenticated

    let state = fromMaybe state' $ T.stripPrefix ":" state'

    let api_env = wxppSubApiEnv sub
    case fmap OAuthCode m_code of
        Just code | not (deniedOAuthCode code) -> do
            -- 用户同意授权
            err_or_atk_info <- tryWxppWsResult $ flip runReaderT api_env $
                                  wxppOAuthGetAccessToken app_id secret code
            atk_info <- case err_or_atk_info of
                            Left err -> do
                                $logErrorS wxppLogSource $
                                    "wxppOAuthGetAccessToken failed: " <> tshow err
                                throwM $ HCError NotAuthenticated

                            Right x -> return x

            now <- liftIO getCurrentTime
            let expiry  = addUTCTime (oauthAtkTTL atk_info) now
                open_id = oauthAtkOpenID atk_info
                scopes  = oauthAtkScopes atk_info
                atk_p = getOAuthAccessTokenPkg (app_id, atk_info)

            let get_union_id1 = MaybeT $ return $ oauthAtkUnionID atk_info
                get_union_id2 = do
                  guard $ any oauthScopeCanGetUserInfo scopes

                  err_or_oauth_user_info <- tryWxppWsResult $ flip runReaderT api_env $ wxppOAuthGetUserInfo' atk_p
                  case err_or_oauth_user_info of
                    Left err -> do $logErrorS wxppLogSource $ "wxppOAuthGetUserInfo' failed: " <> tshow err
                                   mzero

                    Right oauth_user_info -> do
                      liftIO $ wxppCacheAddSnsUserInfo cache app_id "zh_CN" oauth_user_info
                      MaybeT $ return $ oauthUserInfoUnionID oauth_user_info

            m_union_id <- runMaybeT $ get_union_id1 <|> get_union_id2

            liftIO $ wxppCacheAddOAuthAccessToken cache atk_p expiry

            sessionMarkWxppUser app_id open_id m_union_id

            let rdr_url = case parseURI return_url of
                    Just uri ->
                                let qs = uriQuery uri
                                    qs' = case qs of
                                            _   | null qs   -> qs ++ "?"
                                                | qs == "?" -> qs
                                                | otherwise -> qs ++ "&"
                                    new_uri = uri { uriQuery =
                                                        qs' ++ "state=" ++ urlEncode (T.unpack state)
                                                    }
                                in uriToString id new_uri ""

                    _ -> return_url

            --- $logDebugS wxppLogSource $ "redirecting to: " <> T.pack rdr_url
            redirect rdr_url

        _ -> do
            -- 授权失败
            defaultLayoutSub $ do
                $(widgetFileReload def "oauth/user_denied")
-- }}}1


-- | 比较通用的处理从 oauth 重定向回来时的Handler的逻辑
-- 如果用户通过授权，则返回 open id 及 oauth token 相关信息
wxppHandlerOAuthReturnGetInfo :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage, MonadLogger m)
                              => SomeWxppApiBroker
                              -> WxppAppID
                              -> m (Maybe (WxppOpenID, OAuthTokenInfo))
-- {{{1
wxppHandlerOAuthReturnGetInfo broker app_id = do
  m_code <- fmap (fmap OAuthCode) $ runInputGet $ iopt textField "code"
  case m_code of
      Just code | not (deniedOAuthCode code) -> do
          -- 用户同意授权
          err_or_muid <- runExceptT $ do
            err_or_atk_res <- liftIO $ wxppApiBrokerOAuthGetAccessToken broker app_id code
            atk_res <- case err_or_atk_res of
                        Nothing -> do
                          $logErrorS wxppLogSource $ "Broker call failed: no such app"
                          throwError $ asString "no such app"

                        Just (WxppWsResp (Left err)) -> do
                          $logErrorS wxppLogSource $
                              "wxppApiBrokerOAuthGetAccessToken failed: " <> tshow err
                          throwError "wxppApiBrokerOAuthGetAccessToken failed"

                        Just (WxppWsResp (Right x)) -> return x

            let open_id = oauthAtkOpenID atk_res
            now <- liftIO getCurrentTime
            let atk_info = fromOAuthAccessTokenResult now atk_res

            lift $ setSession (sessionKeyWxppUser app_id) (unWxppOpenID open_id)

            return (open_id, atk_info)

          return $ either (const Nothing) Just $ err_or_muid

      _ -> do
        $logError "should never reach here"
        return Nothing
-- }}}1


-- | 测试是否已经过微信用户授权，是则执行执行指定的函数
-- 否则重定向至微信授权页面，待用户授权成功后再重定向回到当前页面
wxppOAuthHandler :: (MonadHandler m, WxppCacheTemp c)
                => c
                -> (Route MaybeWxppSub -> [(Text, Text)] -> m Text)
                -> Maybe WxppAppID
                -- ^ 如果我方是第三方平台中服务方，需指定此参数
                -> WxppAppID
                -> OAuthScope
                -> ( OAuthAccessTokenPkg -> m a )
                -> m a
-- {{{1
wxppOAuthHandler cache render_url m_comp_app_id app_id scope f = do
    m_atk_p <- wxppOAuthHandlerGetAccessTokenPkg cache app_id scope
    case m_atk_p of
        Nothing -> do
            is_wx <- isJust <$> handlerGetWeixinClientVersion
            unless is_wx $ do
                permissionDenied "请用在微信里打开此网页"
            url <- getCurrentUrl
            wxppOAuthLoginRedirectUrl render_url m_comp_app_id app_id scope "" (UrlText url)
                >>= redirect . unUrlText
        Just atk_p -> f atk_p
-- }}}1


wxppOAuthHandlerGetAccessTokenPkg :: (MonadHandler m, WxppCacheTemp c)
                                    => c
                                    -> WxppAppID
                                    -> OAuthScope
                                    -> m (Maybe OAuthAccessTokenPkg)
-- {{{1
wxppOAuthHandlerGetAccessTokenPkg cache app_id scope = do
    m_oauth_st <- sessionGetWxppUser app_id
    case m_oauth_st of
        Nothing         -> return Nothing
        Just open_id    -> do
            m_atk_info <- liftIO $ wxppCacheGetOAuthAccessToken cache
                                        app_id open_id (Set.singleton scope)
            case m_atk_info of
                Nothing         -> return Nothing
                Just atk_info   -> return $ Just (packOAuthTokenInfo app_id open_id atk_info)
-- }}}1

-- | 演示/测试微信 oauth 授权的页面
getOAuthTestR :: HandlerT MaybeWxppSub (HandlerT master IO) Text
getOAuthTestR = withWxppSubHandler $ \sub -> do
    let app_id = getWxppAppID sub
    m_oauth_st <- sessionGetWxppUser app_id
    case m_oauth_st of
        Nothing      -> return "no open id, authorization failed"
        Just open_id -> return $ "Your open id is: " <> unWxppOpenID open_id


checkWaiReqThen ::
    (WxppSub -> HandlerT MaybeWxppSub (HandlerT master IO) a)
    -> HandlerT MaybeWxppSub (HandlerT master IO) a
checkWaiReqThen f = withWxppSubHandler $ \foundation -> withWxppSubLogging foundation $ do
    b <- waiRequest >>= liftIO . (wxppSubTrustedWaiReq $ wxppSubOptions foundation)
    if b
        then f foundation
        else permissionDenied "denied by security check"


mimicInvalidAppID :: HandlerT MaybeWxppSub (HandlerT master IO) a
mimicInvalidAppID = sendResponse $ toJSON $
                        WxppAppError
                            (WxppErrorX $ Right WxppInvalidAppID)
                            "invalid app id"

mimicServerBusy :: Text -> HandlerT MaybeWxppSub (HandlerT master IO) a
mimicServerBusy s = sendResponse $ toJSON $
                        WxppAppError
                            (WxppErrorX $ Right WxppServerBusy)
                            s

forwardWsResult :: (ToJSON a) =>
    String -> Either WxppWsCallError a -> HandlerT MaybeWxppSub (HandlerT master IO) Value
-- {{{1
forwardWsResult op_name res = do
    case res of
        Left (WxppWsErrorApp err) -> do
            sendResponse $ toJSON err

        Left err -> do
            $logError $ fromString $
                op_name ++ " failed: " ++ show err
            mimicServerBusy $ fromString $ op_name ++ " failed"

        Right x -> do
            return $ toJSON x
-- }}}1

-- | 提供 access-token
-- 为重用代码，错误报文格式与微信平台接口一样
-- 逻辑上的返回值是 AccessToken
getGetAccessTokenR :: HandlerT MaybeWxppSub (HandlerT master IO) Value
getGetAccessTokenR = checkWaiReqThen $ \foundation -> do
    alreadyExpired
    liftM toJSON $ getAccessTokenSubHandler' foundation


-- | 找 OpenID 对应的 UnionID
-- 为重用代码，错误报文格式与微信平台接口一样
-- 逻辑上的返回值是 Maybe WxppUnionID
getGetUnionIDR :: WxppOpenID -> HandlerT MaybeWxppSub (HandlerT master IO) Value
-- {{{1
getGetUnionIDR open_id = checkWaiReqThen $ \foundation -> do
    alreadyExpired
    let sm_mode = wxppSubMakeupUnionID $ wxppSubOptions foundation
    if sm_mode
        then do
            return $ toJSON $ Just $ fakeUnionID open_id
        else do
            let app_id = getWxppAppID foundation
            let cache = wxppSubCacheBackend foundation

            (tryWxppWsResult $ liftIO $ wxppCacheLookupUserInfo cache app_id open_id)
                >>= forwardWsResult "wxppCacheLookupUserInfo"
-- }}}1


-- | 初始化 WxppUserCachedInfo 表的数据
getInitCachedUsersR ::
    HandlerT MaybeWxppSub (HandlerT master IO) Value
-- {{{1
getInitCachedUsersR = checkWaiReqThen $ \foundation -> do
    alreadyExpired
    atk <- getAccessTokenSubHandler' foundation

    let api_env = wxppSubApiEnv foundation
    _ <- liftIO $ forkIO $ wxppSubRunLoggingT foundation $ do
          _ <- runWxppDB (wxppSubRunDBAction foundation) $ initWxppUserDbCacheOfApp api_env (return atk)
          return ()

    return $ toJSON ("run in background" :: Text)
-- }}}1


-- | 为客户端调用平台的 wxppQueryEndUserInfo 接口
-- 逻辑返回值是 EndUserQueryResult
getQueryUserInfoR :: WxppOpenID -> HandlerT MaybeWxppSub (HandlerT master IO) Value
-- {{{1
getQueryUserInfoR open_id = checkWaiReqThen $ \foundation -> do
    alreadyExpired
    atk <- getAccessTokenSubHandler' foundation
    let sm_mode = wxppSubMakeupUnionID $ wxppSubOptions foundation
        fix_uid qres =
            if sm_mode
                then endUserQueryResultSetUnionID (Just $ fakeUnionID open_id) <$> qres
                else qres

    tryWxppWsResult
      (flip runReaderT (wxppSubApiEnv foundation) $ wxppQueryEndUserInfo atk open_id)
      >>= return . fix_uid
      >>= forwardWsResult "wxppQueryEndUserInfo"
-- }}}1


-- | 模仿创建永久场景的二维码
-- 行为接近微信平台的接口，区别是
-- 输入仅仅是一个 WxppScene
postCreateQrCodePersistR :: HandlerT MaybeWxppSub (HandlerT master IO) Value
-- {{{1
postCreateQrCodePersistR = checkWaiReqThen $ \foundation -> do
    let api_env = wxppSubApiEnv foundation
    alreadyExpired
    scene <- decodePostBodyAsYaml
    let sm_mode = wxppSubFakeQRTicket $ wxppSubOptions foundation
    if sm_mode
        then do
            qrcode_base_url <- withUrlRenderer $ \render ->
                                    render ShowSimulatedQRCodeR []

            let fake_ticket = (scene, qrcode_base_url)
            return $ toJSON $ C8.unpack $ B64L.encode $ LB.toStrict $ A.encode fake_ticket

        else do
            atk <- getAccessTokenSubHandler' foundation
            flip runReaderT api_env $ do
              liftM toJSON $ wxppQrCodeCreatePersist atk scene
-- }}}1


-- | 返回一个二维码图像
-- 其内容是 WxppScene 用 JSON 格式表示之后的字节流
getShowSimulatedQRCodeR :: HandlerT MaybeWxppSub (HandlerT master IO) TypedContent
-- {{{1
getShowSimulatedQRCodeR = do
    ticket_s <- lookupGetParam "ticket"
                >>= maybe (httpErrorRetryWithValidParams ("missing ticket" :: Text)) return

    (ticket :: FakeQRTicket) <- case B64L.decode (C8.pack $ T.unpack ticket_s) of
        Left _ -> httpErrorRetryWithValidParams ("invalid ticket" :: Text)
        Right bs -> case decodeEither' bs of
                        Left err -> do
                            $logError $ fromString $
                                "cannot decode request body as YAML: " ++ show err
                            sendResponseStatus (mkStatus 449 "Retry With") $
                                ("retry wtih valid request JSON body" :: Text)

                        Right (x, y) -> return (x, UrlText y)

    let scene = fst ticket
    let input = C8.unpack $ LB.toStrict $ A.encode scene
    bs <- encodeStringQRCodeJpeg 5 input
    return $ toTypedContent (typeSvg, toContent bs)
-- }}}1


-- | 返回与输入的 union id 匹配的所有 open id 及 相应的 app_id
getLookupOpenIDByUnionIDR ::
    WxppUnionID
    -> HandlerT WxppSubNoApp (HandlerT master IO) Value
getLookupOpenIDByUnionIDR union_id = checkWaiReqThenNA $ \foundation -> do
    alreadyExpired
    liftM toJSON $ liftIO $ wxppSubNoAppUnionIdByOpenId foundation union_id


-- | 接收第三方平台的事件通知
-- GET 方法用于echo检验
-- POST 方法真正处理业务逻辑
getTpEventNoticeR :: HandlerT WxppTpSub (HandlerT master IO) Text
getTpEventNoticeR = withSiteLogFuncInHandlerT $ do
  foundation <- getYesod
  checkSignature foundation "signature" ""
  runInputGet $ ireq textField "echostr"

postTpEventNoticeR :: Yesod master => HandlerT WxppTpSub (HandlerT master IO) Text
-- {{{1
postTpEventNoticeR = do
  foundation <- getYesod
  req <- waiRequest
  lbs <- liftIO $ lazyRequestBody req

  let enc_key   = LNE.head $ wxppTpSubAesKeys foundation
      my_app_id = wxppTpSubComponentAppId foundation

  err_or_resp <- runExceptT $ do
      encrypted_xml_t <- either throwError return
                          (parse_xml_lbs lbs >>= wxppEncryptedTextInDocumentE)

      lift $ checkSignature foundation "msg_signature" encrypted_xml_t

      decrypted_xml0 <- either throwError (return . fromStrict)
                              (B64.decode (encodeUtf8 encrypted_xml_t) >>= wxppDecrypt my_app_id enc_key)

      let err_or_parsed = parse_xml_lbs decrypted_xml0 >>= wxppTpDiagramFromCursor . fromDocument
      uts_or_notice <- case err_or_parsed of
                        Left err -> do
                            $logErrorS wxppLogSource $ fromString $ "Error when parsing incoming XML: " ++ err
                            throwError "Cannot parse XML document"

                        Right x -> return x

      case uts_or_notice of
        Left unknown_info_type -> do
          $logErrorS wxppLogSource $
            "Failed to handle event notice: unknown InfoType: "
            <> unknown_info_type
          throwError $ "Unknown or unsupported InfoType"

        Right notice -> do
          ExceptT $ wxppTpSubHandlerEventNotice foundation notice

  case err_or_resp of
      Left err -> do
          $(logErrorS) wxppLogSource $ fromString $
              "Cannot handle third-party event notice: " <> err
          throwM $ HCError $
              InternalError "Cannot handle third-party event notice"

      Right output -> do
          return output

  where
      parse_xml_lbs x  = case parseLBS def x of
                              Left ex     -> Left $ "Failed to parse XML: " <> show ex
                              Right xdoc  -> return xdoc
-- }}}1


-- | 第三方平台接收公众号消息与事件的端点入口
-- GET 方法用于通讯检测
-- POST 方法用于业务逻辑
getTpMessageR :: WxppAppID -> HandlerT WxppTpSub (HandlerT master IO) Text
getTpMessageR _auther_app_id = do
  foundation <- getYesod
  checkSignature foundation "signature" ""
  runInputGet $ ireq textField "echostr"

postTpMessageR :: WxppAppID -> HandlerT WxppTpSub (HandlerT master IO) Text
postTpMessageR auther_app_id = do
  foundation <- getYesod
  let comp_app_id = getWxppAppID foundation

  realHandlerMsg foundation (ProcAppThirdParty comp_app_id auther_app_id)


instance Yesod master => YesodSubDispatch MaybeWxppSub (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesMaybeWxppSub)


instance YesodSubDispatch WxppSubNoApp (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesWxppSubNoApp)


instance Yesod master => YesodSubDispatch WxppTpSub (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesWxppTpSub)

--------------------------------------------------------------------------------

checkWaiReqThenNA ::
    (WxppSubNoApp -> HandlerT WxppSubNoApp (HandlerT master IO) a)
    -> HandlerT WxppSubNoApp (HandlerT master IO) a
checkWaiReqThenNA f = do
    foundation <- getYesod
    wxppSubNoAppRunLoggingT foundation $ LoggingT $ \log_func -> do
        withLogFuncInHandlerT log_func $ do
            b <- waiRequest >>= liftIO . wxppSubNoAppCheckWaiReq foundation
            if b
                then f foundation
                else permissionDenied "denied by security check"

decodePostBodyAsYaml :: (FromJSON a) =>
    HandlerT MaybeWxppSub (HandlerT master IO) a
-- {{{1
decodePostBodyAsYaml = do
    body <- rawRequestBody $$ sinkLbs
    case decodeEither' (LB.toStrict body) of
        Left err -> do
            $logError $ fromString $
                "cannot decode request body as YAML: " ++ show err
            sendResponseStatus (mkStatus 449 "Retry With") $
                ("retry wtih valid request JSON body" :: Text)

        Right x -> return x
-- }}}1


getAccessTokenSubHandler' ::
    WxppSub -> HandlerT MaybeWxppSub (HandlerT master IO) AccessToken
getAccessTokenSubHandler' foundation = do
    let cache = wxppSubCacheBackend foundation
    let app_id = getWxppAppID foundation
    (liftIO $ wxppCacheGetAccessToken cache app_id)
            >>= maybe (mimicServerBusy "no access token available") (return . fst)


fakeUnionID :: WxppOpenID -> WxppUnionID
fakeUnionID (WxppOpenID x) = WxppUnionID $ "fu_" <> x


-- | initialize db table: WxppUserCachedInfo
initWxppUserDbCacheOfApp :: ( MonadIO m, MonadLogger m, MonadThrow m)
                         => WxppApiEnv
                         -> m AccessToken
                         -> ReaderT WxppDbBackend m Int
-- {{{1
initWxppUserDbCacheOfApp api_env get_atk = do
    atk <- lift get_atk
    flip runReaderT api_env $ do
      wxppGetEndUserSource' (lift $ lift get_atk)
          =$= CL.concatMap wxppOpenIdListInGetUserResult
          =$= save_to_db atk
          $$ CC.length
    where

        save_to_db atk = awaitForever $ \open_id -> do
            let app_id = accessTokenApp atk
            now <- liftIO getCurrentTime
            _ <- lift $ do
                m_old <- lift $ getBy $ UniqueWxppUserCachedInfo open_id app_id
                case m_old of
                    Just _ -> do
                        -- 假定 open id 及 union id 对于固定的 app 是固定的
                        -- 已有的记录暂时不更新了
                        -- 因为调用平台接口有点慢
                        return ()

                    Nothing -> do
                        qres <- wxppQueryEndUserInfo atk open_id
                        _ <- lift $ insertBy $ WxppUserCachedInfo app_id
                                (endUserQueryResultOpenID qres)
                                (endUserQueryResultUnionID qres)
                                now
                        return ()

                lift transactionSave

            yield ()
-- }}}1


-- | 调用微信 oauth 取 open id　再继续处理下一步逻辑
-- 注意：这里使用当前页面作为微信返回地址，因此query string参数不要与微信的冲突
--       不适用于第三方平台
yesodMakeSureInWxLoggedIn :: ( MonadHandler m, Yesod (HandlerSite m)
                             , RenderMessage (HandlerSite m) FormMessage
                             , MonadLogger m, MonadCatch m
                             , HasWxppUrlConfig e, HasWreqSession e
                             , WxppCacheTemp c
                             )
                          => e
                          -> c
                          -> (WxppAppID -> m (Maybe WxppAppSecret))
                          -> (UrlText -> m UrlText)
                          -- ^ 修改微信返回地址
                          -> OAuthScope
                          -> WxppAppID
                          -> m a
                          -- ^ 确实不能取得 open id　时调用
                          -> (WxppOpenID -> Maybe WxppUnionID -> m a)
                          -- ^ 取得 open_id 之后调用这个函数
                          -- 假定微信的回调参数 code, state 不会影响这部分的逻辑
                          -> m a
-- {{{1
yesodMakeSureInWxLoggedIn wx_api_env cache get_secret fix_return_url scope app_id h_no_id h = do
  m_wx_id <- sessionGetWxppUserU app_id
  case m_wx_id of
    Just (open_id, m_union_id) -> h (getWxppOpenID open_id) m_union_id
    Nothing -> do
      m_code <- fmap (fmap OAuthCode) $ runInputGet $ iopt hiddenField "code"
      case m_code of
        Just code | not (deniedOAuthCode code) -> do
          err_or_wx_id <- runExceptT $ do
            m_state <- lift $ lookupGetParam "state"
            m_expected_state <- lift $ lookupSession (sessionKeyWxppOAuthState app_id)
            unless (m_expected_state == m_state) $ do
              $logError $ "OAuth state check failed, got: " <> tshow m_state
              throwError $ "unexpected state"

            m_secret <- lift $ get_secret app_id
            secret <- case m_secret of
                        Nothing -> do
                          $logError $ "cannot get app secret from cache server"
                          throwError $ asString "no secret"
                        Just x -> return x

            err_or_atk_info <- tryWxppWsResult $ flip runReaderT wx_api_env $
                                wxppOAuthGetAccessToken app_id secret code
            oauth_atk_info <- case err_or_atk_info of
                            Left err -> do
                                $logError $
                                    "wxppOAuthGetAccessToken failed: " <> tshow err
                                throwError "wxppOAuthGetAccessToken failed"

                            Right x -> return x

            let open_id = oauthAtkOpenID oauth_atk_info
                scopes = oauthAtkScopes oauth_atk_info

            if member AS_SnsApiUserInfo scopes
               then do
                  let oauth_atk_pkg = getOAuthAccessTokenPkg (app_id, oauth_atk_info)
                      lang = "zh_CN"

                  oauth_user_info <- flip runReaderT wx_api_env $ wxppOAuthGetUserInfo lang oauth_atk_pkg
                  liftIO $ wxppCacheAddSnsUserInfo cache app_id lang oauth_user_info
                  return $ (open_id, oauthUserInfoUnionID oauth_user_info)

               else do
                  return $ (open_id, Nothing)

          case err_or_wx_id of
            Left _        -> throwM $ userError "微信接口错误，请稍后重试"
            Right (open_id, m_union_id) -> h open_id m_union_id

        _ -> do
          m_state <- lookupGetParam "state"
          if isJust m_state
             then do
               -- 说明当前已经是从微信认证回来的回调
               h_no_id

             else do
               random_state <- wxppOAuthMakeRandomState app_id
               current_url <- getCurrentUrl
               let oauth_return_url    = UrlText $ fromMaybe current_url $ fmap pack $
                                           urlUpdateQueryText
                                               (filter (flip onotElem ["state", "code"] . fst))
                                               (unpack current_url)
               oauth_retrurn_url2  <- fix_return_url oauth_return_url
               let oauth_url = wxppOAuthRequestAuthInsideWx
                                   Nothing -- ^ not third-prty
                                   app_id
                                   scope
                                   oauth_retrurn_url2
                                   random_state

               redirect oauth_url
-- }}}1


-- vim: set foldmethod=marker:
