{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
module WeiXin.PublicPlatform.Yesod.Site
    ( module WeiXin.PublicPlatform.Yesod.Site
    , module WeiXin.PublicPlatform.Yesod.Site.Data
    ) where

import ClassyPrelude
import Yesod
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base16     as B16
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
import Data.Yaml                            (decodeFileEither, encode)


import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Message
import WeiXin.PublicPlatform.Menu
import WeiXin.PublicPlatform.WS


checkSignature :: Yesod master => HandlerT WxppSub (HandlerT master IO) ()
checkSignature = do
    foundation <- getYesod

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

getMessageR :: Yesod master => HandlerT WxppSub (HandlerT master IO) Text
getMessageR = do
    checkSignature
    (httpErrorWhenParamError =<<) $ do
        reqGetParamE' "echostr"

postMessageR :: Yesod master => HandlerT WxppSub (HandlerT master IO) Text
postMessageR = do
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
    foundation <- getYesod
    let app_config  = wxppSubAppConfig foundation
        app_id      = wxppConfigAppID app_config
        ak          = wxppConfigAppAesKey app_config
        bak_aks     = wxppConfigAppBackupAesKeys app_config


    err_or_resp <- lift $ runExceptT $ do
        decrypted_xml <-
            if enc
                then do
                    (either throwE return $ parse_xml_lbs lbs >>= wxppTryDecryptByteStringDocumentE app_id (ak:bak_aks))
                        >>= maybe (throwE $ "Internal Error: Assertion Failed") (return . LB.fromStrict)
                else return lbs

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
                            if enc
                                then ExceptT $ wxppOutMsgEntityToDocumentE
                                                    app_id ak out_msg_entity
                                else return $ wxppOutMsgEntityToDocument out_msg_entity

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
                                Left ex     -> fail $ "Failed to parse XML: " <> show ex
                                Right xdoc  -> return xdoc


-- | reload menu from config/menu.yml
getReloadMenuR :: Yesod master => HandlerT WxppSub (HandlerT master IO) String
getReloadMenuR = do
    err_or_menu <- liftIO $ decodeFileEither "config/menu.yml"
    case err_or_menu of
        Left err    -> do
            $(logErrorS) wxppLogSource $
                "Failed to parse menu yml: " <> fromString (show err)
            return $ "Failed to parse yml: " <> show err
        Right menu  -> do
            foundation <- getYesod
            m_atk <- liftIO $ wxppSubAccessTokens foundation
            case m_atk of
                Nothing             -> return $ "Failed to create menu: no access token available."
                Just access_token   ->  do
                    err_or <- tryWxppWsResult $
                                    if null menu
                                        then wxppDeleteMenu access_token
                                        else wxppCreateMenu access_token menu
                    case err_or of
                        Left err    -> do
                                        $(logErrorS) wxppLogSource $
                                                "Failed to reload menu: " <> fromString (show err)
                                        return $ "Failed to reload menu: " <> show err
                        Right _     -> do
                                        return $ "Menu reloaded successfully."


getQueryMenuR :: Yesod master => HandlerT WxppSub (HandlerT master IO) Text
getQueryMenuR = do
    foundation <- getYesod
    m_atk <- liftIO $ wxppSubAccessTokens foundation
    case m_atk of
        Nothing             -> return $ "Failed to create menu: no access token available."
        Just access_token   ->  do
            err_or <- tryWxppWsResult $ wxppQueryMenu access_token
            case err_or of
                Left err    -> do
                                $(logErrorS) wxppLogSource $
                                        "Failed to query menu: " <> fromString (show err)
                                return $ fromString $ "Failed to query menu: " <> show err
                Right menus -> do
                                return $ decodeUtf8 $ encode menus

instance Yesod master => YesodSubDispatch WxppSub (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesWxppSub)
