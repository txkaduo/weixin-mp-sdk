module WeiXin.PublicPlatform.Pay.Yesod where

import ClassyPrelude.Yesod

import           Control.Monad.Except
import           Data.Conduit.Binary  (sinkLbs)
import           Text.XML             (renderText)
import           Yesod.Helpers.Utils    (randomString)

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


-- | 商户内订单号的推荐算法
-- 订单号须对应某个数据库表id
-- 同时，为减少重复的机会，在前面加上一段随机字母串
-- 这是因为虽然数据库表id本身是唯一的，但出现重复的机会还有很多
-- * 不同的系统共用一个微信接口账号
-- * 相同的系统重新部署使得表id重新计算
wxPayNewUniqueId :: PathPiece a => Int -> a -> IO Text
wxPayNewUniqueId pre_len k = do
  prefix <- randomString pre_len $ ['a'..'z'] <> ['A'..'Z']
  return $ fromString prefix <> "-" <> toPathPiece k


-- vim: set foldmethod=marker:
