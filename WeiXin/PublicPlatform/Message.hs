module WeiXin.PublicPlatform.Message where

import ClassyPrelude hiding (Element)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base64     as B64
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT

import Text.XML
import Text.XML.Cursor
import Text.Hamlet.XML
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types                     (Parser)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Data.Time.Clock.POSIX                ( posixSecondsToUTCTime
                                            , utcTimeToPOSIXSeconds)
import Data.Time                            (NominalDiffTime)
import Numeric                              (readDec, readFloat)

import WeiXin.PublicPlatform.Security


wxppInMsgEntityFromLbs :: LB.ByteString -> Either String WxppInMsgEntity
wxppInMsgEntityFromLbs bs =
    case parseLBS def bs of
        Left ex     -> fail $ "Failed to parse XML: " <> show ex
        Right doc   -> wxppInMsgEntityFromDocument doc


wxppInMsgEntityFromLbsA ::
    WxppAppID
    -> AesKey
    -> LB.ByteString
    -> Either String WxppInMsgEntity
wxppInMsgEntityFromLbsA app_id ak bs =
    case parseLBS def bs of
        Left ex     -> fail $ "Failed to parse XML: " <> show ex
        Right doc   -> wxppInMsgEntityFromDocumentA app_id ak doc


-- | get message from 'Encrypt' element
wxppInMsgEntityFromDocumentE ::
    WxppAppID
    -> AesKey
    -> Document
    -> Either String WxppInMsgEntity
wxppInMsgEntityFromDocumentE app_id ak doc = do
    decrypted_xml <- get_ele_s "Encrypt" >>= decrypt
    case parseLBS def $ LB.fromStrict decrypted_xml of
        Left ex     -> fail $ "Failed to parse XML: " <> show ex
        Right ndoc  -> wxppInMsgEntityFromDocument ndoc
    where
        get_ele_s = getElementContent cursor
        cursor = fromDocument doc
        decrypt t = (B64.decode $ encodeUtf8 t)
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
        Left ex     -> fail $ "Failed to parse XML: " <> show ex
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
                Left ex     -> fail $ "Failed to parse XML: " <> show ex
                Right ndoc  -> wxppInMsgEntityFromDocument ndoc
    where
        cursor = fromDocument doc
        decrypt t = (B64.decode $ encodeUtf8 t)
                            >>= wxppDecrypt app_id ak

wxppInMsgEntityFromDocument :: Document -> Either String WxppInMsgEntity
wxppInMsgEntityFromDocument doc = do
    to_user <- get_ele_s "ToUserName"
    from_user <- fmap WxppOpenID $ get_ele_s "FromUserName"
    tt <- get_ele_s "CreateTime"
                >>= maybe
                        (fail $ "Failed to parse CreateTime")
                        (return . posixSecondsToUTCTime
                                . (realToFrac :: Int64 -> NominalDiffTime)
                        )
                    . simpleParseDecT
    msg_id <- fmap (fmap WxppInMsgID) $
                mapM (maybe (fail $ "Failed to parse MsgId") return . simpleParseDecT)
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
                    url <- get_ele_s "PicUrl"
                    media_id <- fmap WxppMediaID $ get_ele_s "MediaId"
                    return $ WxppInMsgImage media_id url

        "voice" -> do
                    media_id <- fmap WxppMediaID $ get_ele_s "MediaId"
                    format <- get_ele_s "Format"
                    let reg = getElementContentMaybe cursor "Recognition"
                    return $ WxppInMsgVoice media_id format reg

        "video" -> do
                    media_id <- fmap WxppMediaID $ get_ele_s "MediaId"
                    thumb_media_id <- fmap WxppMediaID $ get_ele_s "ThumbMediaId"
                    return $ WxppInMsgVideo media_id thumb_media_id

        "location" -> do
                    x <- get_ele_s "Location_X"
                            >>= maybe (fail $ "Failed to parse Location_X") return
                                . simpleParseFloatT
                    y <- get_ele_s "Location_Y"
                            >>= maybe (fail $ "Failed to parse Location_Y") return
                                . simpleParseFloatT
                    scale <- get_ele_s "Scale"
                            >>= maybe (fail $ "Failed to parse Scale") return
                                . simpleParseFloatT
                    label <- get_ele_s "Label"
                    return $ WxppInMsgLocation (x, y) scale label

        "link"      -> do
                    url <- get_ele_s "Url"
                    title <- get_ele_s "Title"
                    desc <- get_ele_s "Description"
                    return $ WxppInMsgLink url title desc

        "event"     -> fmap WxppInMsgEvent $ wxppEventFromDocument doc

        _       -> fail $ T.unpack $
                    "unknown/unsupported MsgType: " <> msg_type_s

    where
        get_ele_s = getElementContent cursor
        cursor = fromDocument doc


wxppEventFromDocument :: Document -> Either String WxppEvent
wxppEventFromDocument doc = do
    evt_type <- get_ele_s "Event"
    case evt_type of
        "subscribe" -> do
            case getElementContentMaybe cursor "EventKey" of
                Nothing     -> return $ WxppEvtSubscribe
                Just ek_s   -> do
                            let prefix = "qrscene_"
                            ticket <- fmap QRTicket $ get_ele_s "Ticket"
                            scene_id <- liftM WxppSceneID $
                                if T.isPrefixOf prefix ek_s
                                    then
                                        maybe
                                            (fail $ "Failed to parse scene id")
                                            return
                                            $ simpleParseDecT $
                                                T.drop (length prefix) ek_s

                                    else fail $ T.unpack $
                                                "EventKey does not start with "
                                                    <> prefix
                            return $ WxppEvtSubscribeAtScene scene_id ticket

        "unsubscribe" -> return $ WxppEvtUnsubscribe

        "SCAN"      -> do
                    scan_id <- fmap WxppSceneID $
                            get_ele_s "EventKey"
                                >>= maybe
                                        (fail $ "Failed to parse EventKey")
                                        return
                                    . simpleParseDecT
                    ticket <- fmap QRTicket $ get_ele_s "Ticket"
                    return $ WxppEvtScan scan_id ticket

        "LOCATION"  -> do
                    latitude <- get_ele_s "Latitude"
                            >>= maybe (fail $ "Failed to parse Latitude") return
                                . simpleParseFloatT
                    longitude <- get_ele_s "Longitude"
                            >>= maybe (fail $ "Failed to parse Longitude") return
                                . simpleParseFloatT
                    prec <- get_ele_s "Precision"
                            >>= maybe (fail $ "Failed to parse Precision") return
                                . simpleParseFloatT
                    return $ WxppEvtReportLocation (latitude, longitude) prec

        "CLICK" -> do
                    ek <- get_ele_s "EventKey"
                    return $ WxppEvtClickItem ek
        "VIEW"  -> do
                    url <- get_ele_s "EventKey"
                    return $ WxppEvtFollowUrl url

        _       -> fail $ T.unpack $
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
    -> AesKey
    -> WxppOutMsgEntity -> m (Either String Document)
wxppOutMsgEntityToDocumentE app_id ak me = runExceptT $ do
    encrypted_xml <- ExceptT $ wxppEncryptText app_id ak $
                        LT.toStrict $ renderText def plain_xml
    let root = Element "xml" mempty root_nodes
        root_nodes = [xml|
<ToUserName>#{unWxppOpenID $ wxppOutToUserName me}
<Encrypt>#{encrypted_xml}
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
<FromUserName>#{wxppOutFromUserName me}
<CreateTime>#{fromString $ show $ utcTimeToEpochInt $ wxppOutCreatedTime me}
|]

-- | 外发信息对应的节点列表
-- 不包含 CreateTime 之类的与特定信息无关的节点
wxppOutMsgToNodes :: WxppOutMsg -> [Node]

wxppOutMsgToNodes (WxppOutMsgText ct) = [xml|
<MsgType>text
<Content>#{ct}
|]

wxppOutMsgToNodes (WxppOutMsgImage (WxppMediaID media_id)) = [xml|
<MsgType>image
<MediaId>#{media_id}
|]

wxppOutMsgToNodes (WxppOutMsgVoice (WxppMediaID media_id)) = [xml|
<MsgType>voice
<MediaId>#{media_id}
|]

wxppOutMsgToNodes (WxppOutMsgVideo (WxppMediaID media_id) m_title m_desc) = [xml|
<MsgType>video
<MediaId>#{media_id}
$maybe title <- m_title
    <Title>#{title}
$maybe desc <- m_desc
    <Description>#{desc}
|]

wxppOutMsgToNodes (WxppOutMsgMusic (WxppMediaID media_id) m_title m_desc m_url m_hq_url) = [xml|
<MsgType>music
<ThumbMediaId>#{media_id}
$maybe title <- m_title
    <Title>#{title}
$maybe desc <- m_desc
    <Description>#{desc}
$maybe url <- m_url
    <MusicUrl>#{url}
$maybe hq_url <- m_hq_url
    <HQMusicUrl>#{hq_url}
|]

wxppOutMsgToNodes (WxppOutMsgArticle articles) = [xml|
<MsgType>news
<ArticleCount>#{T.pack $ show $ length articles}
<Articles>
    $forall article <- articles
        <Item>
            $maybe title <- wxppArticleTitle article
                <Title>#{title}
            $maybe desc <- wxppArticleDesc article
                <Description>#{desc}
            $maybe url <- wxppArticleUrl article
                <Url>#{url}
            $maybe pic_url <- wxppArticlePicUrl article
                <PicUrl>#{pic_url}
|]


-- | 用于在配置文件中，读取出一系列响应算法
parseJsonWxppInMsgHandlers ::
    [SomeJsonWxppInMsgHandler m]
        -- ^ value inside SomeJsonWxppInMsgHandler is not used
        -- use: SomeJsonWxppInMsgHandler undefined is ok
    -> Value
    -> Parser [SomeJsonWxppInMsgHandler m]
parseJsonWxppInMsgHandlers known_hs = withArray "[SomeJsonWxppInMsgHandler]" $
        mapM (parseJsonWxppInMsgHandler known_hs) . toList


parseJsonWxppInMsgHandler ::
    [SomeJsonWxppInMsgHandler m]
        -- ^ value inside SomeJsonWxppInMsgHandler is not used
        -- use: SomeJsonWxppInMsgHandler undefined is ok
    -> Value
    -> Parser (SomeJsonWxppInMsgHandler m)
parseJsonWxppInMsgHandler known_hs =
    withObject "SomeJsonWxppInMsgHandler" $ \obj -> do
        name <- obj .: "name"
        SomeJsonWxppInMsgHandler h <- maybe
                (fail $ "unknown handler name: " <> T.unpack name)
                return
                $ flip find  known_hs
                $ \(SomeJsonWxppInMsgHandler h) -> isNameOfInMsgHandler (Just h) name
        fmap SomeJsonWxppInMsgHandler $ parseInMsgHandler (Just h) obj


-- | 使用列表里的所有算法，逐一调用一次以处理收到的信息
-- 返回第一个返回 Right Just 的结果
-- 如果都没有返回 Right Just 则：
-- 如果有一个handler返回 Right Nothing，这个函数也返回 Right Nothing
-- （代表成功但无回复）
-- 如果没有一个handler返回 Right Nothing，则：
--   若有返回 Left 的，则选择第一个 Left 返回（代表失败）
--   若连 Left 也没有（那只可能出现在handler数量本身就是零的情况）
--      则理解为成功
tryEveryInMsgByHandler :: MonadLogger m =>
    [WxppInMsgHandler m] -> WxppInMsgHandler m
tryEveryInMsgByHandler handlers ime = do
    (errs, res_lst) <- liftM partitionEithers $
                            mapM (\h -> h ime) handlers
    forM_ errs $ \err -> do
        $(logWarnS) wxppLogSource $ T.pack $
            "Error when handling incoming message, "
            <> "MsgId=" <> (show $ wxppInMessageID ime)
            <> ": " <> err

    case catMaybes res_lst of
        []      -> do
                    -- 没有一个 handler 有回复
                    -- 如果 res_lst 本身也是空，说明全部都失败了，
                    -- 除非errs也为空
                    return $
                        if null res_lst
                            then maybe (Right Nothing) Left $
                                    listToMaybe errs
                            else Right Nothing
        (x:xs)  -> do
                    when (not $ null xs) $ do
                        -- 有多个 Just 结果，但服务器只能让我们回复一个信息
                        -- 因此后面的信息会丢失
                        $(logWarnS) wxppLogSource $ T.pack $
                            "more than one reply messages from handlers,"
                            <> " incoming MsgId=" <> (show $ wxppInMessageID ime)
                    return $ Right $ Just x

--------------------------------------------------------------------------------

utcTimeToEpochInt :: UTCTime -> Int
utcTimeToEpochInt = round . utcTimeToPOSIXSeconds

getElementContent :: Cursor -> Name -> Either String Text
getElementContent cursor t =
    maybe (Left $ T.unpack $ "no such element: " <> nameLocalName t) Right $
        getElementContentMaybe cursor t

getElementContentMaybe :: Cursor -> Name -> Maybe Text
getElementContentMaybe cursor t =
    listToMaybe $ cursor $/ element t &// content

simpleParseDec :: (Eq a, Num a) => String -> Maybe a
simpleParseDec = fmap fst . listToMaybe . filter (null . snd) . readDec

simpleParseDecT :: (Eq a, Num a) => Text -> Maybe a
simpleParseDecT = simpleParseDec . T.unpack

simpleParseFloat :: (Eq a, RealFrac a) => String -> Maybe a
simpleParseFloat = fmap fst . listToMaybe . filter ((== "") . snd) . readFloat

simpleParseFloatT :: (Eq a, RealFrac a) => Text -> Maybe a
simpleParseFloatT = simpleParseFloat . T.unpack
