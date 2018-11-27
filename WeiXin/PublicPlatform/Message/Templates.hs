{-# lANGUAGE RecordWildCards, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module WeiXin.PublicPlatform.Message.Templates where

import ClassyPrelude
import Data.Proxy
import WeiXin.PublicPlatform.Message.Template
import WeiXin.PublicPlatform.Types

type MkPayload t = t -> WxppOpenID -> TemplateMsgSendPayload

class WxTemplate t where
  templateShortId :: WxppMsgTemplateShortID
  templateTitle :: t -> Text
  templateUrl :: t -> Maybe UrlText
  templateMiniProgram :: t -> Maybe MiniProgramLink
  templateData :: t -> [(Text, TemplateVal)]
  withTemplates :: forall m a. (Monad m) => Map WxppMsgTemplateShortID WxppMsgTemplateID -> (MkPayload t -> m a) -> m a
  mkPayload :: WxppMsgTemplateID -> MkPayload t

  withTemplates tpls f = do
    case lookup (templateShortId @t) tpls of
      Nothing -> fail "no template id"
      Just tpl_id -> f $ mkPayload tpl_id

  mkPayload tpl_id tpl user_id =
    TemplateMsgSendPayload
              user_id
              tpl_id
              (templateUrl tpl)
              (templateMiniProgram tpl)
              (templateData tpl)

data PostClassCommentTemplate = PostClassCommentTemplate
  { pcctFirst :: Text
  , pcctStudent :: Text
  , pcctCourse  :: Text
  , pcctTeacher :: Text
  , pcctRemark  :: Text
  , pcctUrl     :: Maybe UrlText
  , pcctMiniProgram :: Maybe MiniProgramLink
  }

instance WxTemplate PostClassCommentTemplate where
  templateShortId = WxppMsgTemplateShortID "OPENTM206165018"
  templateTitle = const "课后点评提醒"
  templateUrl = pcctUrl
  templateMiniProgram = pcctMiniProgram
  templateData PostClassCommentTemplate {..} =
    [ ("first", TemplateVal pcctFirst Nothing)
    , ("keyword1", TemplateVal pcctStudent Nothing)
    , ("keyword2", TemplateVal pcctCourse Nothing)
    , ("keyword3", TemplateVal pcctTeacher Nothing)
    , ("remark", TemplateVal pcctRemark Nothing)
    ]

-- vim: set foldmethod=marker:
