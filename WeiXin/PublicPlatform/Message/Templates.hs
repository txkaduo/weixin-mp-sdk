{-# lANGUAGE RecordWildCards, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module WeiXin.PublicPlatform.Message.Templates where

import ClassyPrelude
#if MIN_VERSION_base(4, 13, 0)
-- import Control.Monad (MonadFail(..))
#else
#endif
import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad.Logger
import Data.Default
import WeiXin.PublicPlatform.Message.Template
import WeiXin.PublicPlatform.Types

type MkPayload t = BaseTemplate -> t -> WxppOpenID -> TemplateMsgSendPayload

class WxTemplate t where
  templateShortId :: WxppMsgTemplateShortID
  templateTitle :: Text
  templateData :: t -> [(Text, TemplateVal)]
  withTemplates :: forall m a. (MonadThrow m, MonadLogger m) => Map WxppMsgTemplateShortID WxppMsgTemplateID -> (MkPayload t -> m a) -> m a
  mkPayload :: WxppMsgTemplateID -> MkPayload t

  withTemplates tpls f = do
    let short_id = templateShortId @t
    case lookup short_id tpls of
      Nothing -> do
        $logError $ "template id not found by short id: " <> tshow short_id
        throwM $ userError "no template id"
      Just tpl_id -> f $ mkPayload tpl_id

  mkPayload tpl_id (BaseTemplate{..}) tpl user_id =
    TemplateMsgSendPayload
              user_id
              tpl_id
              btUrl
              btMiniProgram
              ( templateData tpl
              <> [ ("first", btFirst)
                , ("remark", btRemark)
                ]
              )

data BaseTemplate = BaseTemplate
  { btFirst :: TemplateVal
  , btRemark :: TemplateVal
  , btUrl     :: Maybe UrlText
  , btMiniProgram :: Maybe MiniProgramLink
  }

instance Default BaseTemplate where
  def = BaseTemplate def def def def

plainBaseTemplate :: Text -> Text -> BaseTemplate
plainBaseTemplate f r = def { btFirst = TemplateVal f def, btRemark = TemplateVal r def }


-- vim: set foldmethod=marker:
