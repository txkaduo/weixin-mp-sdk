{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WeiXin.PublicPlatform.Yesod.Site
    ( module WeiXin.PublicPlatform.Yesod.Site
    , module WeiXin.PublicPlatform.Yesod.Site.Data
    ) where

import ClassyPrelude
import Yesod
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64.URL as B64L
import qualified Data.ByteString.Char8      as C8
import qualified Data.Text                  as T
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..), throwE)
import Control.Concurrent.Async             (async)
import Control.Concurrent                   (threadDelay)

import Yesod.Helpers.Handler                ( httpErrorWhenParamError
                                            , reqGetParamE'
                                            , paramErrorFromEither
                                            , httpErrorRetryWithValidParams
                                            )
import Network.Wai                          (lazyRequestBody)
import Text.XML                             (renderText, parseLBS)
import Data.Default                         (def)
import qualified Data.Text.Lazy             as LT
import Yesod.Core.Types                     (HandlerContents(HCError))
import Data.Yaml                            (decodeEither')
import Network.HTTP.Types.Status            (mkStatus)
import Data.Conduit
import Data.Conduit.Binary                  (sinkLbs)
import qualified Data.Aeson                 as A

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)   -- blaze-svg
import qualified Data.QRCode as QR                -- haskell-qrencode
import qualified Diagrams.Backend.SVG as D        -- diagrams-svg
import qualified Diagrams.Prelude as D            -- diagrams-lib
import qualified Diagrams.QRCode as QR            -- diagrams-qrcode

import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Message
import WeiXin.PublicPlatform.Error
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.EndUser
import WeiXin.PublicPlatform.QRCode


checkSignature :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) ()
checkSignature = do
    foundation <- getYesod >>= maybe notFound return . unMaybeWxppSub

    let token = wxppConfigAppToken $ wxppSubAppConfig $ foundation

        check_sign (tt, nn, sign) =
            if B16.encode sign0 == encodeUtf8 ( T.toLower sign )
                then Right ()
                else Left $ "invalid signature"
            where
                sign0 = wxppSignature token tt nn ""

    (httpErrorWhenParamError =<<) $ do
        -- check required params
        sign <- reqGetParamE' "signature"
        tt <- liftM (fmap TimeStampS) $ reqGetParamE' "timestamp"
        nn <- liftM (fmap Nonce) $ reqGetParamE' "nonce"
        let dat = (,,) <$> tt <*> nn <*> sign
            res = dat >>= paramErrorFromEither "signature" . check_sign
        return $ res *> pure ()

getMessageR :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) Text
getMessageR = do
    checkSignature
    (httpErrorWhenParamError =<<) $ do
        reqGetParamE' "echostr"

postMessageR :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) Text
postMessageR = do
    foundation <- getYesod >>= maybe notFound return . unMaybeWxppSub

    checkSignature
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
    let app_config  = wxppSubAppConfig foundation
        app_id      = wxppAppConfigAppID app_config
        aks         = wxppConfigAppAesKey app_config : wxppConfigAppBackupAesKeys app_config
        app_token   = wxppConfigAppToken app_config


    err_or_resp <- lift $ runExceptT $ do
        (decrypted_xml, m_enc_akey) <-
            if enc
                then do
                    (either throwE return $ parse_xml_lbs lbs >>= wxppTryDecryptByteStringDocumentE app_id aks)
                        >>= maybe (throwE $ "Internal Error: Assertion Failed")
                                (return . (LB.fromStrict *** Just))
                else return (lbs, Nothing)

        let err_or_parsed = parse_xml_lbs decrypted_xml >>= wxppInMsgEntityFromDocument
        m_ime <- case err_or_parsed of
                    Left err -> do
                        $logErrorS wxppLogSource $ fromString $ "Error when parsing incoming XML: " ++ err
                        return Nothing
                    Right x -> return $ Just x

        let handle_msg      = wxppSubMsgHandler foundation
        out_res <- ExceptT $
                (try $ liftIO $ wxppSubRunLoggingT foundation $ handle_msg decrypted_xml m_ime)
                    >>= return
                            . either
                                (Left . (show :: SomeException -> String))
                                id

        case m_ime of
            Nothing -> do
                -- incoming message cannot be parsed
                -- we don't know who send the message
                return ("", [])

            Just me -> do
                let user_open_id    = wxppInFromUserName me
                    my_name         = wxppInToUserName me

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
                                            my_name
                                            now
                                            x
                liftM (, map mk_out_msg_entity other_out_msgs) $
                    fmap (fromMaybe "") $ forM m_resp_out_msg $ \out_msg -> do
                        liftM (LT.toStrict . renderText def) $ do
                            let out_msg_entity = mk_out_msg_entity out_msg
                            case m_enc_akey of
                                Just enc_akey ->
                                    ExceptT $ wxppOutMsgEntityToDocumentE
                                                    app_id app_token enc_akey out_msg_entity
                                Nothing ->
                                    return $ wxppOutMsgEntityToDocument out_msg_entity

    case err_or_resp of
        Left err -> do
            $(logErrorS) wxppLogSource $ fromString $
                "cannot encode outgoing message into XML: " <> err
            throwM $ HCError $
                InternalError "cannot encode outgoing message into XML"
        Right (xmls, other_out_msgs) -> do
            when (not $ null other_out_msgs) $ do
                void $ liftIO $ async $ do
                    -- 延迟半秒只要为了让直接回复的回应能第一个到达用户
                    threadDelay $ 1000 * 500
                    wxppSubSendOutMsgs foundation other_out_msgs

            return xmls

    where
        parse_xml_lbs x  = case parseLBS def x of
                                Left ex     -> Left $ "Failed to parse XML: " <> show ex
                                Right xdoc  -> return xdoc


checkWaiReqThen :: Yesod master =>
    HandlerT MaybeWxppSub (HandlerT master IO) a
    -> HandlerT MaybeWxppSub (HandlerT master IO) a
checkWaiReqThen f = do
    foundation <- getYesod >>= maybe notFound return . unMaybeWxppSub
    b <- waiRequest >>= liftIO . (wxppSubTrustedWaiReq $ wxppSubOptions foundation)
    if b
        then f
        else permissionDenied "denied by security check"


mimicInvalidAppID :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) a
mimicInvalidAppID = sendResponse $ toJSON $
                        WxppAppError
                            (WxppErrorX $ Right WxppInvalidAppID)
                            "invalid app id"

mimicServerBusy :: Yesod master => Text -> HandlerT MaybeWxppSub (HandlerT master IO) a
mimicServerBusy s = sendResponse $ toJSON $
                        WxppAppError
                            (WxppErrorX $ Right WxppServerBusy)
                            s

forwardWsResult :: (Yesod master, ToJSON a) =>
    String -> Either WxppWsCallError a -> HandlerT MaybeWxppSub (HandlerT master IO) Value
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

-- | 提供 access-token
-- 为重用代码，错误报文格式与微信平台接口一样
-- 逻辑上的返回值是 AccessToken
getGetAccessTokenR :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) Value
getGetAccessTokenR = checkWaiReqThen $ do
    alreadyExpired
    liftM toJSON $ getAccessTokenSubHandler


-- | 找 OpenID 对应的 UnionID
-- 为重用代码，错误报文格式与微信平台接口一样
-- 逻辑上的返回值是 Maybe WxppUnionID
getGetUnionIDR :: Yesod master => WxppOpenID -> HandlerT MaybeWxppSub (HandlerT master IO) Value
getGetUnionIDR open_id = checkWaiReqThen $ do
    alreadyExpired
    foundation <- getYesod >>= maybe mimicInvalidAppID return . unMaybeWxppSub
    let sm_mode = wxppSubMakeupUnionID $ wxppSubOptions foundation
    if sm_mode
        then do
            return $ toJSON $ Just $ fakeUnionID open_id
        else do
            atk <- getAccessTokenSubHandler' foundation
            (tryWxppWsResult $ liftIO $ wxppSubGetUnionID foundation atk open_id)
                >>= forwardWsResult "wxppSubGetUnionID"


-- | 为客户端调用平台的 wxppQueryEndUserInfo 接口
-- 逻辑返回值是 EndUserQueryResult
getQueryUserInfoR :: Yesod master => WxppOpenID -> HandlerT MaybeWxppSub (HandlerT master IO) Value
getQueryUserInfoR open_id = do
    alreadyExpired
    foundation <- getYesod >>= maybe mimicInvalidAppID return . unMaybeWxppSub
    atk <- getAccessTokenSubHandler' foundation
    let sm_mode = wxppSubMakeupUnionID $ wxppSubOptions foundation
        fix_uid qres =
            if sm_mode
                then endUserQueryResultSetUnionID (Just $ fakeUnionID open_id) <$> qres
                else qres
    (tryWxppWsResult $ wxppQueryEndUserInfo atk open_id)
        >>= return . fix_uid
        >>= forwardWsResult "wxppQueryEndUserInfo"

-- | 模仿创建永久场景的二维码
-- 行为接近微信平台的接口，区别是
-- 输入仅仅是一个 WxppScene
postCreateQrCodePersistR :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) Value
postCreateQrCodePersistR = do
    alreadyExpired
    foundation <- getYesod >>= maybe mimicInvalidAppID return . unMaybeWxppSub
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
            liftM toJSON $ wxppQrCodeCreatePersist atk scene


-- | 返回一个二维码图像
-- 其内容是 WxppScene 用 JSON 格式表示之后的字节流
getShowSimulatedQRCodeR :: Yesod master => HandlerT MaybeWxppSub (HandlerT master IO) TypedContent
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
    qrcode <- liftIO $ QR.encodeString input Nothing QR.QR_ECLEVEL_M QR.QR_MODE_EIGHT True
    let dia = D.scale 6 $ QR.stroke $ QR.pathMatrix $ QR.toMatrix qrcode
        bs = renderSvg $ D.renderDia D.SVG (D.SVGOptions D.Absolute Nothing) dia
    return $ toTypedContent (typeSvg, toContent bs)


-- | 返回与输入的 union id 匹配的所有 open id 及 相应的 app_id
getLookupOpenIDByUnionIDR :: Yesod master =>
    WxppUnionID
    -> HandlerT WxppSubNoApp (HandlerT master IO) Value
getLookupOpenIDByUnionIDR union_id = checkWaiReqThenNA $ do
    alreadyExpired
    foundation <- getYesod
    liftM toJSON $ liftIO $ wxppSubNoAppUnionIdByOpenId foundation union_id


instance Yesod master => YesodSubDispatch MaybeWxppSub (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesMaybeWxppSub)


instance Yesod master => YesodSubDispatch WxppSubNoApp (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesWxppSubNoApp)

--------------------------------------------------------------------------------

checkWaiReqThenNA :: Yesod master =>
    HandlerT WxppSubNoApp (HandlerT master IO) a
    -> HandlerT WxppSubNoApp (HandlerT master IO) a
checkWaiReqThenNA f = do
    foundation <- getYesod
    b <- waiRequest >>= liftIO . wxppSubNoAppCheckWaiReq foundation
    if b
        then f
        else permissionDenied "denied by security check"

decodePostBodyAsYaml :: (Yesod master, FromJSON a) =>
    HandlerT MaybeWxppSub (HandlerT master IO) a
decodePostBodyAsYaml = do
    body <- rawRequestBody $$ sinkLbs
    case decodeEither' (LB.toStrict body) of
        Left err -> do
            $logError $ fromString $
                "cannot decode request body as YAML: " ++ show err
            sendResponseStatus (mkStatus 449 "Retry With") $
                ("retry wtih valid request JSON body" :: Text)

        Right x -> return x


getAccessTokenSubHandler :: Yesod master =>
    HandlerT MaybeWxppSub (HandlerT master IO) AccessToken
getAccessTokenSubHandler = do
    getYesod
        >>= maybe mimicInvalidAppID return . unMaybeWxppSub
        >>= getAccessTokenSubHandler'

getAccessTokenSubHandler' :: Yesod master =>
    WxppSub -> HandlerT MaybeWxppSub (HandlerT master IO) AccessToken
getAccessTokenSubHandler' foundation = do
    (liftIO $ wxppSubAccessTokens foundation)
        >>= maybe (mimicServerBusy "no access token available") return


fakeUnionID :: WxppOpenID -> WxppUnionID
fakeUnionID (WxppOpenID x) = WxppUnionID $ "fu_" <> x
