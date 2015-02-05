{-# OPTIONS_GHC -fno-warn-orphans #-}
module WeiXin.PublicPlatform.Yesod.Site
    ( module WeiXin.PublicPlatform.Yesod.Site
    , module WeiXin.PublicPlatform.Yesod.Site.Data
    ) where

import ClassyPrelude
import Yesod
import qualified Data.ByteString.Base16     as B16
import qualified Data.Text                  as T
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Yesod.Helpers.Handler                ( httpErrorWhenParamError
                                            , reqGetParamE'
                                            , paramErrorFromEither
                                            , httpErrorRetryWithValidParams
                                            )
import Network.Wai                          (lazyRequestBody)
import Text.XML                             (renderText)
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

    let err_or_msg_entity = do
            if enc
                then do
                    wxppInMsgEntityFromLbsET app_id (ak:bak_aks) lbs
                        >>= maybe (fail $ "Internal Error: Assertion Failed") return
                else wxppInMsgEntityFromLbs lbs

    case err_or_msg_entity of
        Left err -> do
                    $(logError) $ fromString $
                        "Failed to parse message from XML: " <> err
                    return ""

        Right me -> do
            let handle_msg      = wxppSubMsgHandler foundation
                user_open_id    = wxppInFromUserName me
                my_name         = wxppInToUserName me

            err_or_resp <- runExceptT $ do
                m_out_msg <- ExceptT $
                        (try $ liftIO $ handle_msg me)
                            >>= return
                                    . either
                                        (Left . (show :: SomeException -> String))
                                        id

                fmap (fromMaybe "") $ forM m_out_msg $ \out_msg -> do
                    now <- liftIO getCurrentTime
                    let out_msg_entity = WxppOutMsgEntity
                                            user_open_id
                                            my_name
                                            now
                                            out_msg
                    liftM (LT.toStrict . renderText def) $
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
                Right xmls -> return xmls


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
