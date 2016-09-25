module WeiXin.PublicPlatform.Pay.Yesod where

import ClassyPrelude.Yesod

import           Control.Monad.Except
import           Data.Conduit.Binary  (sinkLbs)
import           Text.XML             (renderText)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Pay.Types
import WeiXin.PublicPlatform.Pay.Function


-- | 用于实现主动通知接口
yesodHandleWxUserPayNotify :: (MonadHandler m, MonadLogger m, MonadBaseControl IO m)
                           => WxPayAppKey
                           -> ( WxppAppID
                                 -> WxPayMchID
                                 -> WxUserPayStatInfo
                                 -> m ()
                              )
                           -> m RepXml
yesodHandleWxUserPayNotify app_key handle_notify = do
-- {{{1
  lbs <- rawRequestBody $$ sinkLbs
  let parse_params = runExceptT $ do
        input_params <- ExceptT $ wxPayParseInputXmlLbs app_key lbs
        app_id <- fmap WxppAppID $ reqXmlTextField input_params "appid"
        mch_id <- fmap WxPayMchID $ reqXmlTextField input_params "mch_id"
        fmap ((app_id, mch_id),) $ wxUserPayParseStateParams input_params

  err_or <- try parse_params
  out_params <- case err_or of
    Left (WxPayDiagError err) -> do
      $logErrorS wxppLogSource $ "Pay notification: failed to parse: " <> err
      return $ [ ("return_code", "FAIL")
               , ("return_msg", err)
               ]

    Right err_or_pay_stat -> do
      case err_or_pay_stat of
        Left err -> do
          $logErrorS wxppLogSource $ "Pay notification: got result error: " <> tshow err
          return $ [ ("return_code", "FAIL")
                   , ("return_msg", "caller error")
                   ]

        Right ((app_id, mch_id), pay_stat) -> do
          err_or2 <- tryAny $ handle_notify app_id mch_id pay_stat
          case err_or2 of
            Left err -> do
              $logErrorS wxppLogSource $ "Pay notification: failed to handle: " <> tshow err
              return $ [ ("return_code", "FAIL")
                       , ("return_msg", "internal error")
                       ]

            Right () -> do
              return $ [ ("return_code", "SUCCESS")
                       , ("return_msg", "OK")
                       ]

  return $ repXml $ renderText def $ wxPayOutgoingXmlDocFromParams $ mapFromList out_params
-- }}}1


-- vim: set foldmethod=marker:
