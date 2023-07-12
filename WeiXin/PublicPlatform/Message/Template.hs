{-# LANGUAGE DataKinds #-}
module WeiXin.PublicPlatform.Message.Template where

-- {{{1 imports
import           ClassyPrelude hiding (Element)
import           Control.Lens hiding ((.=))
import           Data.Aeson (ToJSON(..), (.=), object, FromJSON(..), withObject, (.:))
import           Data.Aeson.TH (deriveJSON, fieldLabelModifier, defaultOptions)
import           Data.Default
import           Network.Wreq hiding (Proxy)
import qualified Network.Wreq.Session as WS

import           WeiXin.PublicPlatform.Types
import           WeiXin.PublicPlatform.WS
import           WeiXin.PublicPlatform.Utils
-- }}}1


data MiniProgramLink =
  MiniProgramLink
    { miniProgLinkAppID :: WxppMiniProgID
    , miniProgLinkPagePath :: WxppMiniProgID
    }
    deriving (Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = toLower . drop 12 }) ''MiniProgramLink)


data TemplateVal =
  TemplateVal
    { templValValue :: Text
    , templValColor :: Maybe Text
    }
    deriving (Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = toLower . drop 8 }) ''TemplateVal)

instance IsString TemplateVal where
  fromString = flip TemplateVal Nothing . fromString

instance Default TemplateVal where
  def = TemplateVal "" def

data TemplateMsgSendPayload =
  TemplateMsgSendPayload
    { templMsgSendToUser      :: WxppOpenID
    , templMsgSendTemplateID  :: WxppMsgTemplateID
    , templMsgSendUrl         :: Maybe UrlText
    , templMsgSendMiniProgram :: Maybe MiniProgramLink
    , templMsgSendData        :: [(Text, TemplateVal)]
    }
    deriving (Show)

-- {{{1 instances
instance ToJSON TemplateMsgSendPayload where
  toJSON x = object
    [ "touser"      .= templMsgSendToUser x
    , "template_id" .= templMsgSendTemplateID x
    , "url"         .= templMsgSendUrl x
    , "miniprogram" .= templMsgSendMiniProgram x
    , "data"        .= object (map (uncurry (.=)) (map (first aesonKeyFromText) $ templMsgSendData x))
    ]

instance FromJSON TemplateMsgSendPayload where
  parseJSON = withObject "TemplateMsgSendPayload" $ \ o -> do
                TemplateMsgSendPayload <$> o .: "touser"
                                       <*> o .: "template_id"
                                       <*> o .: "url"
                                       <*> o .: "miniprogram"
                                       <*> fmap (mapToList . asMap) (o .: "data")
-- }}}1


newtype RespGetTemplateID = RespGetTemplateID { unRespGetTemplateID :: WxppMsgTemplateID }

instance FromJSON RespGetTemplateID where
  parseJSON = withObject "RespGetTemplateID" $ \ o -> do
    RespGetTemplateID <$> o .: "template_id"


-- | 从企业模板库中选择一个模板消息short id加到自己可以使用的模板集合中，得到一个 WxppMsgTemplateID
wxppGetTemplateIDByShort :: (WxppApiMonad env m)
                    => AccessToken
                    -> WxppMsgTemplateShortID
                    -> m WxppMsgTemplateID
-- {{{1
wxppGetTemplateIDByShort (AccessToken { accessTokenData = atk }) short_id = do
  (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
  let url = wxppUrlConfSecureApiBase url_conf <> "/template/api_add_template"
      opts = defaults & param "access_token" .~ [ atk ]

  liftIO (WS.postWith opts sess url $ object [ "template_id_short" .= short_id ])
              >>= asWxppWsResponseNormal'
              >>= return . unRespGetTemplateID
-- }}}1


newtype RespSendTemplateMsg = RespSendTemplateMsg { unRespSendTemplateMsg :: WxppTemplSendMsgID }

instance FromJSON RespSendTemplateMsg where
  parseJSON = withObject "RespSendTemplateMsg" $ \ o -> do
    RespSendTemplateMsg <$> o .: "msgid"

-- | 发送模板消息
wxppSendTemplateMsg :: (WxppApiMonad env m)
                    => AccessToken
                    -> TemplateMsgSendPayload
                    -> m WxppTemplSendMsgID
-- {{{1
wxppSendTemplateMsg (AccessToken { accessTokenData = atk }) payload = do
  (sess, url_conf) <- asks (getWreqSession &&& getWxppUrlConfig)
  let url = wxppUrlConfSecureApiBase url_conf <> "/message/template/send"
      opts = defaults & param "access_token" .~ [ atk ]

  liftIO (WS.postWith opts sess url $ toJSON payload)
              >>= asWxppWsResponseNormal'
              >>= return . unRespSendTemplateMsg
-- }}}1


-- vim: set foldmethod=marker:
