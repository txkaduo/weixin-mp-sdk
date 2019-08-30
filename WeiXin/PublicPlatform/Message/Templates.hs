{-# lANGUAGE RecordWildCards, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module WeiXin.PublicPlatform.Message.Templates where

import ClassyPrelude
import Control.Monad.Logger
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
  withTemplates :: forall m a. (Monad m, MonadLogger m) => Map WxppMsgTemplateShortID WxppMsgTemplateID -> (MkPayload t -> m a) -> m a
  mkPayload :: WxppMsgTemplateID -> MkPayload t

  mkFirst = flip TemplateVal Nothing
  mkRemark = flip TemplateVal Nothing

  withTemplates tpls f = do
    let short_id = templateShortId @t
    case lookup short_id tpls of
      Nothing -> do
        $logError $ "template id not found by short id: " <> tshow short_id
        fail "no template id"
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
  templateShortId = "OPENTM206165018"
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
  templateShortId = "OPENTM207508286"
  templateTitle = "报告完成提醒"
  templateData ReportTemplate {..} =
    [ ("keyword1", TemplateVal rtType Nothing)
    , ("keyword2", flip TemplateVal Nothing $ tshow rtDate)
    , ("keyword3", TemplateVal rtContent Nothing)
    ]

data CourseTimeChangeTemplate = CourseTimeChangeTemplate
  { ctcTimestamp  :: Text -- XXX: just use text so we do not need to care time zone here
  , ctcFrom       :: Int
  , ctcDelta      :: Int
  , ctcTo         :: Int
  }

instance WxTemplate CourseTimeChangeTemplate where
  templateShortId = "OPENTM400251068"
  templateTitle = "课时变更通知"
  templateData CourseTimeChangeTemplate {..} =
    [ ("keyword1", fromString $ unpack ctcTimestamp)
    , ("keyword2", fromString $ show ctcFrom <> "节")
    , ("keyword3", fromString $ signed ctcDelta <> show ctcDelta <> "节")
    , ("keyword4", fromString $ show ctcTo <> "节")
    ]
      where
        signed x | x > 0 = "+"
        signed x | x < 0 = "-"
        signed _ = ""

-- vim: set foldmethod=marker:
