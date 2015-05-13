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


import WeiXin.PublicPlatform.Yesod.Site.Data
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Message


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
                                Left ex     -> Left $ "Failed to parse XML: " <> show ex
                                Right xdoc  -> return xdoc


instance Yesod master => YesodSubDispatch MaybeWxppSub (HandlerT master IO)
    where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesMaybeWxppSub)
