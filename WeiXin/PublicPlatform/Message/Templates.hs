{-# lANGUAGE RecordWildCards, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module WeiXin.PublicPlatform.Message.Templates where

import ClassyPrelude
import Data.Default
import WeiXin.PublicPlatform.Message.Template
import WeiXin.PublicPlatform.Types

type MkPayload t = BaseTemplate -> t -> WxppOpenID -> TemplateMsgSendPayload

class WxTemplate t where
  templateShortId :: WxppMsgTemplateShortID
  templateTitle :: Text
  mkFirst :: Text -> TemplateVal
  mkRemark :: Text -> TemplateVal
  templateData :: t -> [(Text, TemplateVal)]
  withTemplates :: forall m a. (Monad m) => Map WxppMsgTemplateShortID WxppMsgTemplateID -> (MkPayload t -> m a) -> m a
  mkPayload :: WxppMsgTemplateID -> MkPayload t

  mkFirst = flip TemplateVal Nothing
  mkRemark = flip TemplateVal Nothing

  withTemplates tpls f = do
    case lookup (templateShortId @t) tpls of
      Nothing -> fail "no template id"
      Just tpl_id -> f $ mkPayload tpl_id

  mkPayload tpl_id (BaseTemplate{..}) tpl user_id =
    TemplateMsgSendPayload
              user_id
              tpl_id
              btUrl
              btMiniProgram
              ( templateData tpl
              <> [ ("first", mkFirst @t btFirst)
                , ("remark", mkRemark @t btRemark)
                ]
              )

data BaseTemplate = BaseTemplate
  { btFirst :: Text
  , btRemark :: Text
  , btUrl     :: Maybe UrlText
  , btMiniProgram :: Maybe MiniProgramLink
  }

instance Default BaseTemplate where
  def = BaseTemplate mempty mempty def def

data PostClassCommentTemplate = PostClassCommentTemplate
  { pcctStudent :: Text
  , pcctCourse  :: Text
  , pcctTeacher :: Text
  }

instance WxTemplate PostClassCommentTemplate where
  templateShortId = WxppMsgTemplateShortID "OPENTM206165018"
  templateTitle = "课后点评提醒"
  templateData PostClassCommentTemplate {..} =
    [ ("keyword1", TemplateVal pcctStudent Nothing)
    , ("keyword2", TemplateVal pcctCourse Nothing)
    , ("keyword3", TemplateVal pcctTeacher Nothing)
    ]


data ReportTemplate = ReportTemplate
  { rtType  :: Text
  , rtDate  :: Day
  , rtContent :: Text
  }

instance WxTemplate ReportTemplate where
  templateShortId = WxppMsgTemplateShortID "OPENTM207508286"
  templateTitle = "报告完成提醒"
  templateData ReportTemplate {..} =
    [ ("keyword1", TemplateVal rtType Nothing)
    , ("keyword2", flip TemplateVal Nothing $ tshow rtDate)
    , ("keyword3", TemplateVal rtContent Nothing)
    ]


-- vim: set foldmethod=marker:
