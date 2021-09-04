module WeiXin.PublicPlatform.Message where

import ClassyPrelude hiding (Element)
import Control.Arrow (left)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Base16     as B16
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT

import Text.XML
import Text.XML.Cursor
import Text.Hamlet.XML
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Text.Parsec

import Yesod.Helpers.Parsec                 (SimpleStringRep(..), strictParseSimpleEncoded)

import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Utils
import WeiXin.PublicPlatform.XmlUtils


wxppInMsgEntityFromLbs :: LB.ByteString -> Either String WxppInMsgEntity
wxppInMsgEntityFromLbs bs =
    case parseLBS def bs of
        Left ex     -> Left $ "Failed to parse XML: " <> show ex
        Right doc   -> wxppInMsgEntityFromDocument doc


wxppInMsgEntityFromLbsA ::
    WxppAppID
    -> AesKey
    -> LB.ByteString
    -> Either String WxppInMsgEntity
wxppInMsgEntityFromLbsA app_id ak bs =
    case parseLBS def bs of
        Left ex     -> Left $ "Failed to parse XML: " <> show ex
        Right doc   -> wxppInMsgEntityFromDocumentA app_id ak doc


-- | get "Encrypt" node text
wxppEncryptedTextInDocumentE :: Document -> Either String Text
wxppEncryptedTextInDocumentE doc = do
  get_ele_s "Encrypt"
  where
    get_ele_s = getElementContent cursor
    cursor = fromDocument doc

-- | decrypt bytestring from 'Encrypt' element
wxppDecryptByteStringDocumentE :: WxppAppID
                               -> AesKey
                               -> Document
                               -> Either String ByteString
wxppDecryptByteStringDocumentE app_id ak doc = do
  wxppEncryptedTextInDocumentE doc >>= decrypt
  where
    decrypt t = left T.unpack (B64.decodeBase64 $ encodeUtf8 t)
                        >>= wxppDecrypt app_id ak

-- | call wxppInMsgEntityFromDocumentE with each of the AesKey
wxppTryDecryptByteStringDocumentE ::
    WxppAppID
    -> [AesKey]
    -> Document
    -> Either String (Maybe (ByteString, AesKey))
        -- ^ 如果全部失败，取第一个错误
        -- 如果有一个成功就直接返回成功
        -- 如果失败、成功都没有（只出现在 AesKey 列表为空的情况）
        -- 则为 Right Nothing
wxppTryDecryptByteStringDocumentE app_id ak_list doc =
    case (fails, dones) of
        ([],        []      )   -> Right Nothing
        (_ ,        (done:_))   -> Right $ Just done
        ((err:_),   _       )   -> Left err
    where
        (fails, dones) = partitionEithers $ flip map ak_list $
                            \ak -> (, ak) <$> wxppDecryptByteStringDocumentE app_id ak doc

-- | get message from 'Encrypt' element
wxppInMsgEntityFromDocumentE ::
    WxppAppID
    -> AesKey
    -> Document
    -> Either String WxppInMsgEntity
wxppInMsgEntityFromDocumentE app_id ak doc = do
    decrypted_xml <- get_ele_s "Encrypt" >>= decrypt
    case parseLBS def $ LB.fromStrict decrypted_xml of
        Left ex     -> Left $ "Failed to parse XML: " <> show ex
        Right ndoc  -> wxppInMsgEntityFromDocument ndoc
    where
        get_ele_s = getElementContent cursor
        cursor = fromDocument doc
        decrypt t = left T.unpack (B64.decodeBase64 $ encodeUtf8 t)
                            >>= wxppDecrypt app_id ak

-- | call wxppInMsgEntityFromDocumentE with each of the AesKey
wxppInMsgEntityFromDocumentET ::
    WxppAppID
    -> [AesKey]
    -> Document
    -> Either String (Maybe WxppInMsgEntity)
        -- ^ 如果全部失败，取第一个错误
        -- 如果有一个成功就直接返回成功
        -- 如果失败、成功都没有（只出现在 AesKey 列表为空的情况）
        -- 则为 Right Nothing
wxppInMsgEntityFromDocumentET app_id ak_list doc =
    case (fails, dones) of
        ([],        []      )   -> Right Nothing
        (_ ,        (done:_))   -> Right $ Just done
        ((err:_),   _       )   -> Left err
    where
        (fails, dones) = partitionEithers $ flip map ak_list $
                            \ak -> wxppInMsgEntityFromDocumentE app_id ak doc

wxppInMsgEntityFromLbsET ::
    WxppAppID
    -> [AesKey]
    -> LB.ByteString
    -> Either String (Maybe WxppInMsgEntity)
        -- ^ 如果全部失败，取第一个错误
        -- 如果有一个成功就直接返回成功
        -- 如果失败、成功都没有（只出现在 AesKey 列表为空的情况）
        -- 则为 Right Nothing
wxppInMsgEntityFromLbsET app_id ak_list lbs =
    case parseLBS def lbs of
        Left ex     -> Left $ "Failed to parse XML: " <> show ex
        Right doc   -> wxppInMsgEntityFromDocumentET app_id ak_list doc


-- | get message from 'Encrypt' element if it exists,
-- otherwise use wxppInMsgEntityFromDocument directly
wxppInMsgEntityFromDocumentA ::
    WxppAppID
    -> AesKey
    -> Document -> Either String WxppInMsgEntity
wxppInMsgEntityFromDocumentA app_id ak doc = do
    case getElementContentMaybe cursor "Encrypt" of
        Nothing             -> wxppInMsgEntityFromDocument doc
        Just encrypted_xml  -> do
            decrypted_xml <- decrypt encrypted_xml
            case parseLBS def $ LB.fromStrict decrypted_xml of
                Left ex     -> Left $ "Failed to parse XML: " <> show ex
                Right ndoc  -> wxppInMsgEntityFromDocument ndoc
    where
        cursor = fromDocument doc
        decrypt t = left T.unpack (B64.decodeBase64 $ encodeUtf8 t)
                            >>= wxppDecrypt app_id ak

wxppInMsgEntityFromDocument :: Document -> Either String WxppInMsgEntity
wxppInMsgEntityFromDocument doc = do
    to_user <- fmap WeixinUserName $ get_ele_s "ToUserName"
    from_user <- fmap WxppOpenID $ get_ele_s "FromUserName"
    tt <- get_ele_s "CreateTime"
                >>= maybe
                        (Left $ "Failed to parse CreateTime")
                        (return . epochIntToUtcTime)
                    . simpleParseDecT
    msg_id <- fmap (fmap WxppInMsgID) $
                mapM (maybe (Left $ "Failed to parse MsgId") return . simpleParseDecT)
                    $ getElementContentMaybe cursor "MsgId"
    msg <- wxppInMsgFromDocument doc
    return $ WxppInMsgEntity to_user from_user tt msg_id msg
    where
        get_ele_s = getElementContent cursor
        cursor = fromDocument doc


wxppInMsgFromDocument :: Document -> Either String WxppInMsg
wxppInMsgFromDocument doc = do
    msg_type_s <- get_ele_s "MsgType"
    case msg_type_s of

        "text"  -> do
                    ct <- get_ele_s "Content"
                    return $ WxppInMsgText ct

        "image" -> do
                    url <- UrlText <$> get_ele_s "PicUrl"
                    media_id <- fmap WxppBriefMediaID $ get_ele_s "MediaId"
                    return $ WxppInMsgImage media_id url

        "voice" -> do
                    media_id <- fmap WxppBriefMediaID $ get_ele_s "MediaId"
                    format <- get_ele_s "Format"
                    let reg = getElementContentMaybe cursor "Recognition"
                    return $ WxppInMsgVoice media_id format reg

        "video" -> do
                    media_id <- fmap WxppBriefMediaID $ get_ele_s "MediaId"
                    thumb_media_id <- fmap WxppBriefMediaID $ get_ele_s "ThumbMediaId"
                    return $ WxppInMsgVideo media_id thumb_media_id

        "shortvideo" -> do
                    media_id <- fmap WxppBriefMediaID $ get_ele_s "MediaId"
                    thumb_media_id <- fmap WxppBriefMediaID $ get_ele_s "ThumbMediaId"
                    return $ WxppInMsgShortVideo media_id thumb_media_id

        "location" -> do
                    x <- get_ele_s "Location_X"
                            >>= maybe (Left $ "Failed to parse Location_X") return
                                . simpleParseFloatT
                    y <- get_ele_s "Location_Y"
                            >>= maybe (Left $ "Failed to parse Location_Y") return
                                . simpleParseFloatT
                    scale <- get_ele_s "Scale"
                            >>= maybe (Left $ "Failed to parse Scale") return
                                . simpleParseFloatT
                    loc_label <- get_ele_s "Label"
                    return $ WxppInMsgLocation (x, y) scale loc_label

        "link"      -> do
                    url <- UrlText <$> get_ele_s "Url"
                    title <- get_ele_s "Title"
                    desc <- get_ele_s "Description"
                    return $ WxppInMsgLink url title desc

        "event"     -> fmap WxppInMsgEvent $ wxppEventFromDocument doc

        _       -> Left $ T.unpack $
                    "unknown/unsupported MsgType: " <> msg_type_s

    where
        get_ele_s = getElementContent cursor
        cursor = fromDocument doc


wxppEventFromDocument :: Document -> Either String WxppEvent
wxppEventFromDocument doc = do
    evt_type <- get_ele_s "Event"
    case evt_type of
        "subscribe" -> do
            let ek_s = fromMaybe "" $ getElementContentMaybe cursor "EventKey"
            if null ek_s
                then do
                    -- 实测证明，普通的订阅事件通知也会发个 EventKey 过来，只是为空而已
                    return WxppEvtSubscribe
                else do
                    let ticket = fmap QRTicket $ getElementContentMaybe cursor "Ticket"
                    scene_id <- case parse simpleParser "" ek_s of
                        Left err    -> Left $ "Failed to parse scene id: " ++ show err
                        Right sid   -> return sid

                    return $ WxppEvtSubscribeAtScene scene_id ticket

        "unsubscribe" -> return $ WxppEvtUnsubscribe

        "SCAN"      -> do
                    ek_s <- get_ele_s "EventKey"
                    scene_id <- case parse simpleParser "" ek_s of
                        Left err    -> Left $ "Failed to parse scene id: " ++ show err
                        Right sid   -> return sid
                    let ticket = fmap QRTicket $ getElementContentMaybe cursor "Ticket"
                    return $ WxppEvtScan scene_id ticket

        "LOCATION"  -> do
                    latitude <- get_ele_s "Latitude"
                            >>= maybe (Left $ "Failed to parse Latitude") return
                                . simpleParseFloatT
                    longitude <- get_ele_s "Longitude"
                            >>= maybe (Left $ "Failed to parse Longitude") return
                                . simpleParseFloatT
                    prec <- get_ele_s "Precision"
                            >>= maybe (Left $ "Failed to parse Precision") return
                                . simpleParseFloatT
                    return $ WxppEvtReportLocation (latitude, longitude) prec

        "CLICK" -> do
                    ek <- get_ele_s "EventKey"
                    return $ WxppEvtClickItem ek
        "VIEW"  -> do
                    url <- get_ele_s "EventKey"
                    return $ WxppEvtFollowUrl $ UrlText url

        -- XXX: scancode_push 事件文档并没有描述消息报文的细节，这是根据实测结果描述的
        "scancode_push" -> do
                    ek <- get_ele_s "EventKey"
                    scan_info <- maybe (Left "ScanCodeInfo element not found") return $
                                    listToMaybe $ cursor $/ element "ScanCodeInfo"
                    scan_type <- getElementContent scan_info "ScanType"
                    scan_result <- getElementContent scan_info "ScanResult"
                    return $ WxppEvtScanCodePush ek scan_type scan_result

        -- XXX: scancode_waitmsg 事件文档并没有描述消息报文的细节，这是根据实测结果描述的
        "scancode_waitmsg" -> do
                    ek <- get_ele_s "EventKey"
                    scan_info <- maybe (Left "ScanCodeInfo element not found") return $
                                    listToMaybe $ cursor $/ element "ScanCodeInfo"
                    scan_type <- getElementContent scan_info "ScanType"
                    scan_result <- getElementContent scan_info "ScanResult"
                    return $ WxppEvtScanCodeWaitMsg ek scan_type scan_result

        "MASSSENDJOBFINISH" -> do
                    status <- get_ele_s "Status"
                                >>= left (\x -> "failed to parse Status: " ++ show x)
                                    . strictParseSimpleEncoded
                    total <- get_ele_s "TotalCount"
                                >>= maybe (Left $ "failed to parse TotalCount") Right
                                        . simpleParseDecT
                    f_cnt <- get_ele_s "FilterCount"
                                >>= maybe (Left $ "failed to parse FilterCount") Right
                                        . simpleParseDecT
                    sent_cnt <- get_ele_s "SentCount"
                                >>= maybe (Left $ "failed to parse SentCount") Right
                                        . simpleParseDecT
                    err_cnt <- get_ele_s "ErrorCount"
                                >>= maybe (Left $ "failed to parse ErrorCount") Right
                                        . simpleParseDecT
                    return $ WxppEvtGroupSendReport
                                status total f_cnt sent_cnt err_cnt

        "TEMPLATESENDJOBFINISH" -> do
                    msg_id <- get_ele_s "MsgID"
                                >>= maybe (Left $ "failed to parse MsgID") Right
                                        . simpleParseDecT

                    status <- get_ele_s "Status"
                    m_err_msg <- if status == "success"
                                    then pure Nothing
                                    else case T.stripPrefix "failed:" status of
                                           Nothing -> Left $ "failed to parse Status: " <> unpack status
                                           Just msg -> pure $ Just msg

                    return $ WxppEvtTemplateSendJobFinish (WxppTemplSendMsgID msg_id) m_err_msg


        _       -> Left $ T.unpack $
                    "unknown/unsupported Event type: " <> evt_type

    where
        get_ele_s = getElementContent cursor
        cursor = fromDocument doc


-- | 外发信息对应的未加密xml
wxppOutMsgEntityToDocument :: WxppOutMsgEntity -> Document
wxppOutMsgEntityToDocument me = Document (Prologue [] Nothing []) root []
    where
        root = wxppOutMsgEntityToElement me

-- | 外发信息对应的加密xml
wxppOutMsgEntityToDocumentE :: MonadIO m =>
    WxppAppID
    -> Token
    -> AesKey
    -> WxppOutMsgEntity -> m (Either String Document)
wxppOutMsgEntityToDocumentE app_id app_token ak me = runExceptT $ do
    (encrypted_xml, nonce) <- ExceptT $ wxppEncryptText app_id ak $
                        LT.toStrict $ renderText def plain_xml
    now <- liftIO getCurrentTime
    let ts = TimeStampS $ fromString $ show $ utcTimeToEpochInt now
    let sign = wxppSignature app_token ts nonce encrypted_xml
    let root = Element "xml" mempty root_nodes
        root_nodes = [xml|
<Encrypt>#{encrypted_xml}
<MsgSignature>#{B16.encodeBase16 $ sign}
<TimeStamp>#{unTimeStampS ts}
<Nonce>#{unNounce nonce}
|]
    return $ Document (Prologue [] Nothing []) root []
    where
        plain_xml = wxppOutMsgEntityToDocument me

wxppOutMsgEntityToElement :: WxppOutMsgEntity -> Element
wxppOutMsgEntityToElement me = Element "xml" mempty $ common_nodes <> msg_nodes
    where
        msg_nodes = wxppOutMsgToNodes $ wxppOutMessage me
        common_nodes = [xml|
<ToUserName>#{unWxppOpenID $ wxppOutToUserName me}
<FromUserName>#{unWeixinUserName $ wxppOutFromUserName me}
<CreateTime>#{fromString $ show $ utcTimeToEpochInt $ wxppOutCreatedTime me}
|]

-- | 外发信息对应的节点列表
-- 不包含 CreateTime 之类的与特定信息无关的节点
wxppOutMsgToNodes :: WxppOutMsg -> [Node]

wxppOutMsgToNodes (WxppOutMsgText ct) = [xml|
<MsgType>text
<Content>#{ct}
|]

wxppOutMsgToNodes (WxppOutMsgImage media_id) = [xml|
<MsgType>image
<MediaId>#{unWxppMediaID media_id}
|]

wxppOutMsgToNodes (WxppOutMsgVoice media_id) = [xml|
<MsgType>voice
<MediaId>#{unWxppMediaID media_id}
|]

wxppOutMsgToNodes (WxppOutMsgVideo media_id m_thumb_media_id m_title m_desc) = [xml|
-- XXX: _thumb_media_id 在客服发送接口里有出现，但没有在“回复”接口文档里出现
-- 目前仿照音乐消息的element格式写到XML去
<MsgType>video
<MediaId>#{unWxppMediaID media_id}
$maybe thumb_media_id <- m_thumb_media_id
    <ThumbMediaId>#{unWxppMediaID thumb_media_id}
$maybe title <- m_title
    <Title>#{title}
$maybe desc <- m_desc
    <Description>#{desc}
|]

wxppOutMsgToNodes (WxppOutMsgMusic media_id m_title m_desc m_url m_hq_url) = [xml|
<MsgType>music
<ThumbMediaId>#{unWxppMediaID media_id}
$maybe title <- m_title
    <Title>#{title}
$maybe desc <- m_desc
    <Description>#{desc}
$maybe url <- unUrlText <$> m_url
    <MusicUrl>#{url}
$maybe hq_url <- unUrlText <$> m_hq_url
    <HQMusicUrl>#{hq_url}
|]

wxppOutMsgToNodes (WxppOutMsgNews articles) = [xml|
<MsgType>news
<ArticleCount>#{T.pack $ show $ length articles}
<Articles>
    $forall article <- articles
        <item>
            $maybe title <- wxppArticleTitle article
                <Title>#{title}
            $maybe desc <- wxppArticleDesc article
                <Description>#{desc}
            $maybe url <- unUrlText <$> wxppArticleUrl article
                <Url>#{url}
            $maybe pic_url <- unUrlText <$> wxppArticlePicUrl article
                <PicUrl>#{pic_url}
|]

wxppOutMsgToNodes WxppOutMsgTransferToCustomerService = [xml|
<MsgType>transfer_customer_service
|]
