module WeiXin.PublicPlatform.Pay.Function where

import ClassyPrelude

-- {{{1 imports
import           Control.Lens           hiding ((.=))
import           Control.Monad.Except   hiding (forM_)
import           Control.Monad.Logger
import           Control.Monad.Reader   (asks)
import           Control.Monad.Trans.Maybe
import qualified Crypto.Hash.MD5        as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy   as LB
import           Data.Default           (def)
import           Data.Monoid            (Endo (..))
import           Data.Aeson             (object, (.=), encode, ToJSON(..))
import qualified Data.Text.Lazy         as LT
import           Data.Time              (LocalTime, hoursToTimeZone,
                                         localTimeToUTC, FormatTime)
import           Data.Time.Format       (parseTimeM)
import           Network.Wreq           (responseBody)
import qualified Network.Wreq.Session   as WS
import           Text.XML               (Document (..), Element (..), Name (..),
                                         Node (..), Prologue (..), renderText, parseLBS)
import           Text.XML.Cursor        (child, content, fromDocument, fromNode
                                        , node, ($/), ($|), (&|)
                                        )

import           Text.Parsec.TX.Utils   (SimpleStringRep (..), SimpleEncode(..), parseMaybeSimpleEncoded)
import           Yesod.Helpers.Utils    (nullToNothing)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Utils      (utcTimeToEpochInt)
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Pay.Types
import WeiXin.PublicPlatform.Pay.BankCode
-- }}}1


-- | result code 可以理解为非永久性错误的情况
-- CAUTION: 这个列表很难完全准确，而且是否永久性错误有时间也很模糊
--          还有一个问题是同一个代码在不同的api返回时意义未必一样
--          大致的标准是：
--          * 参数错误，算是永久性的
--          * 环境参数错误，报文错误（理解了程序的bug），签名错误(bug)，算是暂时的
--
--          需要根据实用的需要不断完善
wxPayErrorCodeIsResumable :: WxPayErrorCode -> Bool
-- {{{1
wxPayErrorCodeIsResumable (WxPayErrorCode ec) =
  case ec of
    "NOAUTH"      -> True
    "NO_AUTH"     -> True
    "NOTENOUGH"   -> True
    "SYSTEMERROR" -> True
    "SIGN_ERROR"  -> True
    "XML_ERROR"   -> True
    "CA_ERROR"    -> True
    "FREQ_LIMIT"  -> True
    _             -> False
-- }}}1


wxPayCallErrorCode :: WxPayCallError -> Maybe WxPayErrorCode
wxPayCallErrorCode (WxPayCallErrorResult (WxPayCallResultError ec _)) = Just ec
wxPayCallErrorCode _                                                  = Nothing

-- | 支付单不存在的错误代码是经常要特别处理的
wxPayCallErrorIsDoesNotExist :: WxPayCallError -> Bool
wxPayCallErrorIsDoesNotExist = (== Just (WxPayErrorCode "ORDERNOTEXIST")) . wxPayCallErrorCode


-- | 查询状态为成功时应能提供支付成功通知接口中核心提供的相同数据
toWxUserPaySucceededInfo :: WxUserPayQueryInfo
                         -> Either Text WxUserPaySuccInfo
-- {{{1
toWxUserPaySucceededInfo x = do
  open_id <- maybe (Left "missing openid") return $ wxUserPayQueryOpenId x
  trade_type <- maybe (Left "missing trade_type") return $ wxUserPayQueryTradeType x
  bank_code <- maybe (Left "missing bank_code") return $ wxUserPayQueryBankCode x
  total_fee <- maybe (Left "missing total_fee") return $ wxUserPayQueryTotalFee x
  trans_id <- maybe (Left "missing transaction_id") return $ wxUserPayQueryTransId x
  end_time <- maybe (Left "missing end_time") return $ wxUserPayQueryTimeEnd x

  return $ WxUserPaySuccInfo
    (wxUserPayQueryDeviceInfo x)
    open_id
    (wxUserPayQueryIsSubs x)
    trade_type
    bank_code
    total_fee
    (wxUserPayQuerySettlementTotalFee x)
    (wxUserPayQueryCashFee x)
    (wxUserPayQueryCouponFee x)
    trans_id
    (wxUserPayQueryOutTradeNo x)
    (wxUserPayQueryAttach x)
    end_time
-- }}}1



-- | 微信签名算法
wxPaySignInternal :: WxPayAppKey
                  -> [(Text, Text)]
                  -> WxPaySignature
-- {{{1
wxPaySignInternal (WxPayAppKey ak) params_all =
  WxPaySignature $ toUpper $ fromString $
    C8.unpack $ B16.encode $ MD5.hash $ encodeUtf8 str_to_sign
  where
    mks k v     = mconcat [ k, "=", v ]
    str_to_sign = intercalate "&" $
                    fmap (uncurry mks) $
                      filter (not . null . snd) $
                        (sortBy (comparing fst) params_all) <> [("key", ak)]
-- }}}1


-- | 微信JSSDK chooseWXPay 或旧版接口 WeixinJSBridge的getBrandWCPayRequest所需的参数对象
data WxPayJsArgs = WxPayJsArgs
-- {{{1 fields and ops
  { wxPayJsArgsSignature :: WxPaySignature
  , wxPayJsArgsAppId     :: WxppAppID
  , wxPayJsArgsNonce     :: Nonce
  , wxPayJsArgsTimeStamp :: Int64
  , wxPayJsArgsPackage   :: Text
  , wxPayJsArgsSignType  :: Text
  }

instance ToJSON WxPayJsArgs where
  toJSON x = object [ "appId" .= wxPayJsArgsAppId x
                    , "timeStamp" .= tshow ( wxPayJsArgsTimeStamp x )
                    , "timestamp" .= wxPayJsArgsTimeStamp x
                    -- duplicate to make it compatible with both chooseWXPay and getBrandWCPayRequest
                    , "nonceStr" .= unNounce (wxPayJsArgsNonce x)
                    , "package" .= wxPayJsArgsPackage x
                    , "signType" .= wxPayJsArgsSignType x
                    , "paySign" .= unWxPaySignature (wxPayJsArgsSignature x)
                    ]
-- }}}1

-- | generate signature for JS API: chooseWXPay
wxPayJsSign :: WxPayAppKey
            -> WxppAppID
            -> UTCTime
            -> Nonce
            -> WxUserPayPrepayId
            -> WxPayJsArgs
-- {{{1
wxPayJsSign ak app_id t nonce@(Nonce nonce_str) prepay_id =
  WxPayJsArgs
    (wxPaySignInternal ak params_all)
    app_id
    nonce
    ts
    pkg_str
    sign_type
  where
    sign_type = "MD5"
    ts = utcTimeToEpochInt t
    pkg_str = "prepay_id=" <> unWxUserPayPrepayId prepay_id
    params_all = [ ("appId", unWxppAppID app_id)
                 , ("timeStamp", tshow ts)
                 , ("nonceStr", nonce_str)
                 , ("package", pkg_str)
                 , ("signType", sign_type)
                 ]
-- }}}1


-- | generate signature for XML diagrams
wxPayXmlSign :: WxPayAppKey
             -> WxPayParams
             -- ^ not including: nonce_str, key
             -> Nonce
             -> WxPaySignature
-- {{{1
wxPayXmlSign ak params (Nonce nonce_str) =
  wxPaySignInternal ak (mapToList params_all)
  where
    params_all  = insertMap "nonce_str" nonce_str $ params
-- }}}1


-- | 微信支付调用XML
wxPayOutgoingXmlDoc :: WxPayAppKey
                    -> WxPayParams
                    -- ^ not including: nonce_str, key
                    -> Nonce
                    -> Document
-- {{{1
wxPayOutgoingXmlDoc app_key params nonce@(Nonce raw_nonce) =
  wxPayOutgoingXmlDocFromParams $ params <> mapFromList [ param_nonce, param_sign ]
  where
    param_nonce  = ("nonce_str", raw_nonce)
    sign        = wxPayXmlSign app_key params nonce
    param_sign   = ("sign", unWxPaySignature sign)
-- }}}1


-- | for internal use: make a XML from all key-value pairs
wxPayOutgoingXmlDocFromParams :: WxPayParams
                              -- ^ INCLUDING: nonce_str, key
                              -> Document
-- {{{1
wxPayOutgoingXmlDocFromParams params =
  Document (Prologue [] Nothing []) root []
  where
    root        = Element "xml" mempty nodes
    nodes       = map (uncurry mk_node) (mapToList params)
    mk_node k v = NodeElement $
                    Element (Name k Nothing Nothing) mempty
                      [ NodeContent v ]
-- }}}1

wxPayRenderOutgoingXmlDoc :: WxPayAppKey
                          -> WxPayParams
                          -- ^ not including: nonce_str, key
                          -> Nonce
                          -> LT.Text
wxPayRenderOutgoingXmlDoc app_key params nonce =
  renderText def $ wxPayOutgoingXmlDoc app_key params nonce


-- | 初步解释收到的报文
wxPayParseIncomingXmlDoc :: Maybe WxPayAppKey -- ^ 如果是Nothing，则不检查签名
                         -> Document
                         -> Either Text (Either WxPayCallError WxPayParams)
-- {{{1
wxPayParseIncomingXmlDoc m_app_key doc = do
  (ret_code, params1) <- pop_up_find "return_code" all_params
  if ret_code /= "SUCCESS"
     then do
          let m_err_msg = lookup "return_msg" params1
          return $ Left $ WxPayCallErrorReturn $ WxPayCallReturnError m_err_msg

     else do
          let req_param name = maybe (Left $ "'" <> name <> "' not found: ") return (lookup name all_params )
          result_code <- req_param "result_code"
          if result_code /= "SUCCESS"
             then do
                  err_code <- fmap WxPayErrorCode $ req_param "err_code"
                  err_desc <- req_param "err_code_des"
                  return $ Left $ WxPayCallErrorResult $ WxPayCallResultError err_code err_desc

             else do
                  forM_ m_app_key $ check_signaure
                  return $ Right $ deleteMap "result_code" params1

  where
    cursor = fromDocument doc

    all_params = mapFromList $
                  catMaybes $ map param_from_node $
                    cursor $| child &| node

    check_signaure app_key = do
      (nonce, params1) <- fmap (first Nonce) $ pop_up_find "nonce_str" all_params
      (sign, params2) <- fmap (first WxPaySignature) $ pop_up_find "sign" params1
      let params = params2
      let sign2 = wxPayXmlSign app_key params nonce

      unless (sign2 == sign) $ do
        Left $ "incorrect signature"

    pop_up_find name ps = do
      let (m_matched, unmatched) = (lookup name &&& deleteMap name) ps

      matched_one <- maybe (Left $ "'" <> name <> "' not found: " <> tshow ps) return m_matched
      return (matched_one, unmatched)

    param_from_node n@(NodeElement ele) = do
      v <- listToMaybe $ fromNode n $/ content
      let name = nameLocalName (elementName ele)
      return (name, v)

    param_from_node _ = Nothing
-- }}}1


-- | 统一下单接口
wxUserPayPrepay :: WxppApiMonad env m
                => WxPayCommonParams
                -> UrlText             -- ^ 通知地址
                -> WxPayMoneyAmount
                -> WxUserPayOutTradeNo     -- ^ 商户订单号
                -> WxPayTradeType      -- ^ 交易类型
                -> WxPayParamIpStr     -- ^ 终端IP
                -> WxPayParamBody
                -> [WxPayGoodsDetail]
                -> Maybe WxPayProductId
                -> Maybe WxppOpenID    -- ^ required, if trade_type == JSAPI
                -> Maybe Text          -- ^ 附加数据
                -> m ( (Either WxPayCallError WxPayPrepayOk)
                     , (LB.ByteString, LB.ByteString)
                     )
-- {{{1
wxUserPayPrepay common_params notify_url amount out_trade_no trade_type ip_str body details m_prod_id m_open_id m_attach = do
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfUserPayApiBase url_conf <> "/unifiedorder"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $ catMaybes
                    [ Just $ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Just $ Endo $ insertMap "appid" (unWxppAppID app_id)
                    , Just $ Endo $ insertMap "out_trade_no" (unWxUserPayOutTradeNo out_trade_no)
                    , Just $ Endo $ insertMap "body" $ renderWxPayParamBody body
                    , Just $ Endo $ insertMap "detail" $ toStrict $ decodeUtf8 $ encode $ object [ "goods_detail" .= details ]
                    , fmap (Endo . insertMap "attach") m_attach
                    , Just $ Endo $ insertMap "total_fee" $ tshow $ unWxPayMoneyAmount amount
                    , Just $ Endo $ insertMap "spbill_create_ip" (unWxPayParamIpStr ip_str)
                    , Just $ Endo $ insertMap "notify_url" $ unUrlText notify_url
                    , Just $ Endo $ insertMap "trade_type" $ pack $ simpleEncode trade_type
                    , fmap (Endo . insertMap "product_id" . unWxPayProductId) m_prod_id
                    , fmap (Endo . insertMap "openid" . unWxppOpenID) m_open_id
                    ])

  wxPayCallInternalHelper (WxCallSignReqInOut app_key) url params $ \resp_params -> runExceptT $ do
    let req_param = withExceptT WxPayCallErrorDiag . ExceptT . reqXmlTextField resp_params

    prepay_id <- fmap WxUserPayPrepayId $ req_param "prepay_id"
    trade_type_t <- req_param "trade_type"
    r_trade_type <- case parseMaybeSimpleEncoded trade_type_t of
                    Nothing -> throwError $ WxPayCallErrorDiag $ WxPayDiagError $
                                    "Unknown trade_type: " <> trade_type_t
                    Just x  -> return x

    let m_code_url = fmap UrlText $ lookup "code_url" resp_params

    return $ WxPayPrepayOk prepay_id r_trade_type m_code_url

    where
      WxPayCommonParams app_id app_key mch_id = common_params
-- }}}1


-- | 用户支付：查询接口
wxUserPayQueryOrder :: WxppApiMonad env m
                    => WxPayCommonParams
                    -> Either WxUserPayTransId WxUserPayOutTradeNo
                    -> m ( Either WxPayCallError
                                (WxUserPayQueryInfo, (WxUserPayStatus, Maybe Text))
                           , (LB.ByteString, LB.ByteString)
                         )
                    -- ^ 额外的信息包括 trade_state, trade_state_desc
-- {{{1
wxUserPayQueryOrder common_params trans_id_trade_no = do
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfUserPayApiBase url_conf <> "/orderquery"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $
                    [ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Endo $ insertMap "appid" (unWxppAppID app_id)
                    , Endo $ case trans_id_trade_no of
                              Left trans_id -> insertMap "transaction_id" (unWxUserPayTransId trans_id)
                              Right out_trade_no -> insertMap "out_trade_no" (unWxUserPayOutTradeNo out_trade_no)
                    ])

  wxPayCallInternalHelper (WxCallSignReqInOut app_key) url params $ \resp_params -> runExceptT $ do
    pay_state <- withExceptT WxPayCallErrorDiag $ ExceptT $ wxUserPayParseQueryParams resp_params

    trade_state_t <- withExceptT WxPayCallErrorDiag $ ExceptT $ reqXmlTextField resp_params "trade_state"
    trade_state <- case trade_state_t of
                    "SUCCESS"    -> return WxUserPaySuccess
                    "REFUND"     -> return WxUserPayRefund
                    "NOTPAY"     -> return WxUserPayNotPay
                    "CLOSED"     -> return WxUserPayClosed
                    "REVOKED"    -> return WxUserPayRevoked
                    "USERPAYING" -> return WxUserPayUserPaying
                    "PAYERROR"   -> return WxUserPayPayError
                    _            -> do $logErrorS wxppLogSource $ "Invalid trade_state: " <> trade_state_t
                                       throwError $ WxPayCallErrorDiag $ WxPayDiagError $
                                         "Invalid trade_state: " <> trade_state_t

    let trade_state_desc = lookup "trade_state_desc" resp_params

    return (pay_state, (trade_state, trade_state_desc))
    where
      WxPayCommonParams app_id app_key mch_id = common_params
-- }}}1


-- | 用户支付：关闭支付单
wxUserPayCloseOrder :: WxppApiMonad env m
                    => WxPayCommonParams
                    -> WxUserPayOutTradeNo
                    -> m ( Either WxPayCallError ()
                         , (LB.ByteString, LB.ByteString)
                         )
-- {{{1
wxUserPayCloseOrder common_params out_trade_no = do
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfUserPayApiBase url_conf <> "/closeorder"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $
                    [ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Endo $ insertMap "appid" (unWxppAppID app_id)
                    , Endo $ insertMap "out_trade_no" (unWxUserPayOutTradeNo out_trade_no)
                    ])

  wxPayCallInternalHelper (WxCallSignReqInOut app_key) url params $ \_resp_params -> runExceptT $ do
    return ()

  where
    WxPayCommonParams app_id app_key mch_id = common_params
-- }}}1


-- | 用户支付：申请退款
-- CAUTION: 目前未实现双向数字证书认证
--          实用上的解决方法是使用反向代理(例如HAProxy)提供双向证书认证,
--          我们这里只发起普通的http/https请求
wxUserPayRefund :: WxppApiMonad env m
                => WxPayCommonParams
                -> Text   -- ^ 操作员帐号
                -> Either WxUserPayTransId WxUserPayOutTradeNo
                -> WxUserPayOutRefundNo
                -> WxPayMoneyAmount
                -> WxPayMoneyAmount
                -> m ( Either WxPayCallError WxUserPayRefundReqResult
                     , (LB.ByteString, LB.ByteString)
                     )
-- {{{1
wxUserPayRefund common_params op_user_id trans_id_trade_no refund_no total_fee refund_fee = do
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfUserPayApiSecBase url_conf <> "/refund"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $
                    [ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Endo $ insertMap "appid" (unWxppAppID app_id)
                    , Endo $ case trans_id_trade_no of
                              Left trans_id -> insertMap "transaction_id" (unWxUserPayTransId trans_id)
                              Right out_trade_no -> insertMap "out_trade_no" (unWxUserPayOutTradeNo out_trade_no)
                    , Endo $ insertMap "out_refund_no" (unWxUserPayOutRefundNo refund_no)
                    , Endo $ insertMap "total_fee" (tshow $ unWxPayMoneyAmount total_fee)
                    , Endo $ insertMap "refund_fee" (tshow $ unWxPayMoneyAmount refund_fee)
                    , Endo $ insertMap "op_user_id" op_user_id
                    ])

  wxPayCallInternalHelper (WxCallSignReqInOut app_key) url params $ \resp_params -> runExceptT $ do
    withExceptT WxPayCallErrorDiag $ ExceptT $ wxUserPayParseRefundReqParams resp_params

  where
    WxPayCommonParams app_id app_key mch_id = common_params
-- }}}1


-- | 查询退款时，可用这几种方式指定查询的订单
data RefundQueryOrderId = RefundQueryOrderByTransId WxUserPayTransId
                        | RefundQueryOrderByOutTradeNo WxUserPayOutTradeNo
                        | RefundQueryOrderByOutRefundNo WxUserPayOutRefundNo
                        | RefundQueryOrderByRefundId WxUserPayRefundId

-- | 用户支付：查询退款
wxUserPayRefundQuery :: WxppApiMonad env m
                     => WxPayCommonParams
                     -> Maybe WxPayDeviceInfo
                     -> RefundQueryOrderId
                     -> m ( Either WxPayCallError WxUserPayRefundQueryResult
                          , (LB.ByteString, LB.ByteString)
                          )
-- {{{1
wxUserPayRefundQuery common_params m_dev_info query_order_id = do
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfUserPayApiBase url_conf <> "/refundquery"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $ catMaybes
                    [ Just $ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Just $ Endo $ insertMap "appid" (unWxppAppID app_id)
                    , flip fmap m_dev_info $ \dev -> Endo $ insertMap "device_info" (unWxPayDeviceInfo dev)

                    , Just $ Endo $ case query_order_id of
                              RefundQueryOrderByTransId trans_id ->
                                insertMap "transaction_id" (unWxUserPayTransId trans_id)
                              RefundQueryOrderByOutTradeNo out_trade_no ->
                                insertMap "out_trade_no" (unWxUserPayOutTradeNo out_trade_no)
                              RefundQueryOrderByOutRefundNo out_refund_no ->
                                insertMap "out_refund_no" (unWxUserPayOutRefundNo out_refund_no)
                              RefundQueryOrderByRefundId refund_id ->
                                insertMap "refund_id" (unWxUserPayRefundId refund_id)
                    ])

  wxPayCallInternalHelper (WxCallSignReqInOut app_key) url params $ \resp_params -> runExceptT $ do
    withExceptT WxPayCallErrorDiag $ ExceptT $ wxUserPayParseRefundQueryResult resp_params

  where
    WxPayCommonParams app_id app_key mch_id = common_params
-- }}}1


-- | 微信企业支付
-- CAUTION: 目前未实现双向数字证书认证
--          实用上的解决方法是使用反向代理(例如HAProxy)提供双向证书认证,
--          我们这里只发起普通的http/https请求
wxPayMchTransfer :: (WxppApiMonad env m)
                 => WxPayCommonParams
                 -> Maybe WxPayDeviceInfo
                 -> WxMchTransMchNo
                 -> WxppOpenID
                 -> WxCheckName
                 -> WxPayMoneyAmount
                 -> Text          -- ^ description
                 -> WxPayParamIpStr          -- ^ ip address
                 -> m ( Either WxPayCallError WxPayTransOk
                      , (LB.ByteString, LB.ByteString)
                      )
-- {{{1
wxPayMchTransfer common_params m_dev_info mch_trade_no open_id check_name pay_amount desc ip_str = do
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfMmPayApiBase url_conf <> "/promotion/transfers"

  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $ catMaybes
                    [ Just $ Endo $ insertMap "mchid" (unWxPayMchID mch_id)
                    , Just $ Endo $ insertMap "mch_appid" (unWxppAppID app_id)
                    , flip fmap m_dev_info $ \dev -> Endo $ insertMap "device_info" (unWxPayDeviceInfo dev)

                    , Just $ Endo $ insertMap "partner_trade_no" (unWxMchTransMchNo mch_trade_no)

                    , Just $ Endo $ insertMap "openid" (unWxppOpenID open_id)

                    , case check_name of
                        WxNoCheckName ->
                          Just $ Endo $ insertMap "check_name" "NO_CHECK"

                        WxOptCheckName name ->
                          Just $ mconcat
                                  [ Endo $ insertMap "check_name" "OPTION_CHECK"
                                  , Endo $ insertMap "re_user_name" name
                                  ]

                        WxReqCheckName name ->
                          Just $ mconcat
                                  [ Endo $ insertMap "check_name" "FORCE_CHECK"
                                  , Endo $ insertMap "re_user_name" name
                                  ]

                    , Just $ Endo $ insertMap "amount" (tshow $ unWxPayMoneyAmount pay_amount)
                    , Just $ Endo $ insertMap "desc" desc
                    , Just $ Endo $ insertMap "spbill_create_ip" (unWxPayParamIpStr ip_str)
                    ])


  wxPayCallInternalHelper (WxCallSignReqOut app_key) url params $ \resp_params -> runExceptT $ do
    let req_param = withExceptT WxPayCallErrorDiag . ExceptT . reqXmlTextField resp_params

    mch_out_trade_no <- fmap WxMchTransMchNo $ req_param "partner_trade_no"
    unless (mch_out_trade_no == mch_trade_no) $ do
      throwError $ WxPayCallErrorDiag $ WxPayDiagError $
                "Unexpected response data: partner_trade_no is not the same as input: "
                <> tshow mch_out_trade_no

    wx_trade_no <- fmap WxMchTransWxNo $ req_param "payment_no"
    pay_time <- withExceptT WxPayCallErrorDiag $ ExceptT $ reqXmlTimeField resp_params wxPayMchTransParseTimeStr "payment_time"

    return $ WxPayTransOk mch_out_trade_no wx_trade_no pay_time

  where
    WxPayCommonParams app_id app_key mch_id = common_params
-- }}}1


-- | 微信企业支付结果查询
-- CAUTION: 目前未实现双向数字证书认证
--          实用上的解决方法是使用反向代理(例如HAProxy)提供双向证书认证,
--          我们这里只发起普通的http/https请求
wxPayMchTransferInfo :: (WxppApiMonad env m)
                     => WxPayCommonParams
                     -> WxMchTransMchNo
                     -> m ( Either WxPayCallError WxPayMchTransInfo
                          , (LB.ByteString, LB.ByteString)
                          )
-- {{{1
wxPayMchTransferInfo common_params mch_trade_no = do
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfMmPayApiBase url_conf <> "/gettransferinfo"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $ catMaybes
                    [ Just $ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Just $ Endo $ insertMap "appid" (unWxppAppID app_id)
                    , Just $ Endo $ insertMap "partner_trade_no" (unWxMchTransMchNo mch_trade_no)
                    ])

  wxPayCallInternalHelper (WxCallSignReqOut app_key) url params $ \resp_params -> runExceptT $ do
    let req_param = withExceptT WxPayCallErrorDiag . ExceptT . reqXmlTextField resp_params

    mch_out_trade_no <- fmap WxMchTransMchNo $ req_param "partner_trade_no"
    unless (mch_out_trade_no == mch_trade_no) $ do
      throwError $ WxPayCallErrorDiag $ WxPayDiagError $
                "Unexpected response data: partner_trade_no is not the same as input: "
                <> tshow mch_out_trade_no

    -- 关于 detai_id 的意义不明，暂时认为这就是之前的 payment_no
    wx_trade_no <- fmap WxMchTransWxNo $ req_param "detail_id"

    st <- req_param "status"
    status <- case st of
                "SUCCESS"     -> return WxMmTransStatusSccess
                "PROCESSING"  -> return WxMmTransStatusProcessing
                "FAILED"      -> WxMmTransStatusFailed <$> req_param "reason"
                _             -> throwError $ WxPayCallErrorDiag $ WxPayDiagError $
                                  "status is recognized: " <> st

    open_id <- WxppOpenID <$> req_param "openid"
    let m_recv_name = lookup "transfer_name" resp_params

    amount <- to_call_err $ reqXmlFeeField resp_params "payment_amount"

    trans_time <- to_call_err $ reqXmlTimeField resp_params wxPayMchTransParseTimeStr "transfer_time"

    desc <- req_param "desc"

    return $ WxPayMchTransInfo
                mch_out_trade_no
                wx_trade_no
                status
                open_id
                m_recv_name
                amount
                trans_time
                desc

  where
    WxPayCommonParams app_id app_key mch_id = common_params
    to_call_err = withExceptT WxPayCallErrorDiag . ExceptT
-- }}}1


-- | 微信支付查询接口与主动通知时的报文的共同部分
-- 查询接口与主动通知接口得到的报文有以下的区别
-- * 通知接口多了: appid, mch_id
-- * 查询接口多了: trade_state, trade_state_desc (似乎通知接口默认是成功的)
wxUserPayParseStateParams :: (MonadLogger m)
                          => WxPayParams
                          -> m (Either WxPayDiagError WxUserPaySuccInfo)
-- {{{1
wxUserPayParseStateParams resp_params = runExceptT $ do
  let m_dev_info = fmap WxPayDeviceInfo $ lookup "device_info" resp_params

  open_id <- fmap WxppOpenID $ req_param "openid"
  let m_is_subs_t = lookup "is_subscribe" resp_params
  m_is_subs <- forM m_is_subs_t $ \is_subs_t ->
                 case toUpper is_subs_t of
                   "Y" -> return True
                   "N" -> return False
                   _ -> do
                     $logErrorS wxppLogSource $ "invalid value of 'is_subscribe': " <> is_subs_t
                     throwError $ WxPayDiagError "invalid value of 'is_subscribe'"

  trade_type_t <- req_param "trade_type"
  trade_type <- case parseMaybeSimpleEncoded trade_type_t of
                   Nothing -> throwError $ WxPayDiagError $ "Unknown trade_type: " <> trade_type_t
                   Just x  -> return x

  bank_type_t <- req_param "bank_type"
  bank_type <- case parseBankCode bank_type_t of
                 Just x  -> return $ Right x
                 Nothing -> do
                   $logWarnS wxppLogSource $ "Unknown bank_type: " <> bank_type_t
                   return $ Left bank_type_t

  total_fee <- ExceptT $ reqXmlFeeField resp_params "total_fee"
  settlement_total_fee <- ExceptT $ optXmlFeeField resp_params "settlement_total_fee"
  cash_fee <- ExceptT $ optXmlFeeField resp_params "cash_fee"
  coupon_fee <- ExceptT $ optXmlFeeField resp_params "coupon_fee"
  time_end <- ExceptT $ reqXmlTimeField resp_params wxUserPayParseTimeStr "time_end"

  trans_id <- fmap WxUserPayTransId $ req_param "transaction_id"
  out_trade_no <- fmap WxUserPayOutTradeNo $ req_param "out_trade_no"

  let m_attach = lookup "attach" resp_params

  return $ WxUserPaySuccInfo
                     m_dev_info
                     open_id
                     m_is_subs
                     trade_type
                     bank_type
                     total_fee
                     settlement_total_fee
                     cash_fee
                     coupon_fee
                     trans_id
                     out_trade_no
                     m_attach
                     time_end

  where
    req_param = ExceptT . reqXmlTextField resp_params
-- }}}1


wxUserPayParseQueryParams :: (MonadLogger m)
                          => WxPayParams
                          -> m (Either WxPayDiagError WxUserPayQueryInfo)
-- {{{1
wxUserPayParseQueryParams resp_params = runExceptT $ do
  let m_dev_info = fmap WxPayDeviceInfo $ lookup "device_info" resp_params

  let m_open_id = fmap WxppOpenID $ lookup "openid" resp_params
  let m_is_subs_t = lookup "is_subscribe" resp_params
  m_is_subs <- forM m_is_subs_t $ \is_subs_t ->
                 case toUpper is_subs_t of
                   "Y" -> return True
                   "N" -> return False
                   _ -> do
                     $logErrorS wxppLogSource $ "invalid value of 'is_subscribe': " <> is_subs_t
                     throwError $ WxPayDiagError "invalid value of 'is_subscribe'"


  m_trade_type <- runMaybeT $ do
    trade_type_t <- MaybeT $ return $ lookup "trade_type" resp_params
    case parseMaybeSimpleEncoded trade_type_t of
      Nothing -> lift $ throwError $ WxPayDiagError $ "Unknown trade_type: " <> trade_type_t
      Just x  -> return x

  m_bank_type <- runMaybeT $ do
    bank_type_t <- MaybeT $ return $ lookup "bank_type" resp_params
    case parseBankCode bank_type_t of
      Just x  -> return $ Right x
      Nothing -> do
        $logErrorS wxppLogSource $ "Unknown bank_type: " <> bank_type_t
        return $ Left bank_type_t

  total_fee <- ExceptT $ optXmlFeeField resp_params "total_fee"
  settlement_total_fee <- ExceptT $ optXmlFeeField resp_params "settlement_total_fee"
  cash_fee <- ExceptT $ optXmlFeeField resp_params "cash_fee"
  coupon_fee <- ExceptT $ optXmlFeeField resp_params "coupon_fee"
  time_end <- ExceptT $ optXmlTimeField resp_params wxUserPayParseTimeStr "time_end"

  let trans_id = fmap WxUserPayTransId $ lookup "transaction_id" resp_params

  out_trade_no <- fmap WxUserPayOutTradeNo $ req_param "out_trade_no"

  let m_attach = lookup "attach" resp_params

  return $ WxUserPayQueryInfo
                     m_dev_info
                     m_open_id
                     m_is_subs
                     m_trade_type
                     m_bank_type
                     total_fee
                     settlement_total_fee
                     cash_fee
                     coupon_fee
                     trans_id
                     out_trade_no
                     m_attach
                     time_end

  where
    req_param = ExceptT . reqXmlTextField resp_params
-- }}}1


wxUserPayParseRefundReqParams :: (MonadLogger m)
                              => WxPayParams
                              -> m (Either WxPayDiagError WxUserPayRefundReqResult)
-- {{{1
wxUserPayParseRefundReqParams resp_params = runExceptT $ do
  let m_dev_info = fmap WxPayDeviceInfo $ lookup "device_info" resp_params

  trans_id <- fmap WxUserPayTransId $ req_param "transaction_id"
  out_trade_no <- fmap WxUserPayOutTradeNo $ req_param "out_trade_no"
  refund_id <- fmap WxUserPayRefundId $ req_param "refund_id"
  out_refund_no <- fmap WxUserPayOutRefundNo $ req_param "out_refund_no"

  total_fee <- ExceptT $ reqXmlFeeField resp_params "total_fee"
  refund_fee <- ExceptT $ reqXmlFeeField resp_params "refund_fee"
  settlement_refund_fee <- ExceptT $ optXmlFeeField resp_params "settlement_refund_fee"
  cash_fee <- ExceptT $ reqXmlFeeField resp_params "cash_fee"
  cash_refund_fee <- ExceptT $ optXmlFeeField resp_params "cash_refund_fee"

  channel <- mapM (ExceptT . wxUserPayParseRefundChannelText) $
                join $ fmap nullToNothing $ lookup "refund_channel" resp_params

  return $ WxUserPayRefundReqResult
                    m_dev_info
                    trans_id
                    out_trade_no
                    refund_id
                    out_refund_no
                    total_fee
                    refund_fee
                    settlement_refund_fee
                    cash_fee
                    cash_refund_fee
                    channel

  where
    req_param = ExceptT . reqXmlTextField resp_params
-- }}}1


wxUserPayParseRefundChannelText :: (MonadLogger m)
                                => Text
                                -> m (Either WxPayDiagError WxPayRefundChannel)
-- {{{1
wxUserPayParseRefundChannelText x = runExceptT $ do
  case x of
    "ORIGINAL" -> return WxPayRefundOriginal
    "BALANCE"  -> return WxPayRefundBalance
    _          -> throwError $ WxPayDiagError $ "Unknown refund_channel: " <> x
-- }}}1


wxUserPayParseCouponTypeText :: (MonadLogger m)
                             => Text
                             -> m (Either WxPayDiagError WxPayCouponType)
-- {{{1
wxUserPayParseCouponTypeText x = runExceptT $ do
  case x of
    "CASH"    -> return WxPayCouponCash
    "NO_CASH" -> return WxPayCouponNonCash
    _         -> throwError $ WxPayDiagError $ "Unknown coupon_type: " <> x
-- }}}1


wxUserPayParseRefundAccount :: (MonadLogger m)
                            => Text
                            -> m (Either WxPayDiagError WxPayRefundAccount)
-- {{{1
wxUserPayParseRefundAccount x = runExceptT $ do
  case x of
    "REFUND_SOURCE_RECHARGE_FUNDS"  -> return WxPayRefundAccountRechargeFunds
    "REFUND_SOURCE_UNSETTLED_FUNDS" -> return WxPayRefundAccountUnsettledFunds
    _                               -> throwError $ WxPayDiagError $ "Unknown refund_account: " <> x
-- }}}1


wxUserPayParseRefundQueryItem :: (MonadLogger m)
                              => Int
                              -> WxPayParams
                              -> m (Either WxPayDiagError WxUserPayRefundQueryItem)
-- {{{1
wxUserPayParseRefundQueryItem n resp_params = runExceptT $ do
  out_refund_no <- fmap WxUserPayOutRefundNo $ req_param $ "out_refund_no" <> suffix
  refund_id <- fmap WxUserPayRefundId $ req_param $ "refund_id" <> suffix

  status <- case join $ fmap nullToNothing $ lookup ("refund_status" <> suffix) resp_params of
              Nothing           -> throwError $ WxPayDiagError $ "missing " <> ("refund_status" <> suffix)
              Just "SUCCESS"    -> return WxPayRefundSuccess
              Just "FAIL"       -> return WxPayRefundFail
              Just "PROCESSING" -> return WxPayRefundProcessing
              Just "CHANGE"     -> return WxPayRefundChange
              Just x            -> throwError $ WxPayDiagError $ "Unknown refound_status: " <> x

  channel <- mapM (ExceptT . wxUserPayParseRefundChannelText) $
                join $ fmap nullToNothing $ lookup ("refund_channel" <> suffix) resp_params

  refund_fee <- ExceptT $ reqXmlFeeField resp_params $ "refund_fee" <> suffix
  settlement_refund_fee <- ExceptT $ optXmlFeeField resp_params $ "settlement_refund_fee" <> suffix

  -- accout is a typo, but it is correct element name in response xml
  recv_account <- req_param $ "refund_recv_accout" <> suffix

  coupon_type <- mapM (ExceptT . wxUserPayParseCouponTypeText) $
                    join $ fmap nullToNothing $ lookup ("coupon_type" <> suffix) resp_params

  let coupon_refound_count = join $ fmap readMay $ lookup ("coupon_refund_count" <> suffix) resp_params

  items <- case coupon_refound_count of
             Just m | m > 0 -> mapM (\x -> ExceptT $ wxUserPayParseRefundQueryCouponItem n x resp_params) [0 .. m-1]
             _              -> return []

  return $ WxUserPayRefundQueryItem
            out_refund_no
            refund_id
            status
            channel
            refund_fee
            settlement_refund_fee
            recv_account
            coupon_type
            items
  where
    req_param = ExceptT . reqXmlTextField resp_params
    suffix = "_" <> tshow n
-- }}}1


wxUserPayParseRefundQueryCouponItem :: (MonadLogger m)
                                    => Int
                                    -> Int
                                    -> WxPayParams
                                    -> m (Either WxPayDiagError WxPayRefundQueryCouponRefundItem)
-- {{{1
wxUserPayParseRefundQueryCouponItem n m resp_params = runExceptT $ do
  batch_id <- req_param $ "coupon_refund_batch_id" <> suffix
  refund_id <- req_param $ "coupon_refund_id" <> suffix
  fee <- ExceptT $ reqXmlFeeField resp_params $ "coupon_refund_fee" <> suffix
  return $ WxPayRefundQueryCouponRefundItem batch_id refund_id fee
  where
    req_param = ExceptT . reqXmlTextField resp_params
    suffix = "_" <> tshow n <> "_" <> tshow m
-- }}}1


wxUserPayParseRefundQueryResult :: (MonadLogger m)
                                => WxPayParams
                                -> m (Either WxPayDiagError WxUserPayRefundQueryResult)
-- {{{1
wxUserPayParseRefundQueryResult resp_params = runExceptT $ do
  let m_dev_info = fmap WxPayDeviceInfo $ lookup "device_info" resp_params

  trans_id <- fmap WxUserPayTransId $ req_param "transaction_id"
  out_trade_no <- fmap WxUserPayOutTradeNo $ req_param "out_trade_no"

  total_fee <- ExceptT $ reqXmlFeeField resp_params "total_fee"
  settlement_total_fee <- ExceptT $ optXmlFeeField resp_params "settlement_total_fee"
  cash_fee <- ExceptT $ reqXmlFeeField resp_params "cash_fee"

  refund_account <- mapM (ExceptT . wxUserPayParseRefundAccount) $ lookup "refund_account" resp_params

  refund_count_t <- req_param "refund_count"
  let refund_count = readMay refund_count_t
  items <- case refund_count of
             Nothing -> do
               throwError $ WxPayDiagError $ "Invalid refund_count: " <> refund_count_t

             Just n | n > 0 -> mapM (\x -> ExceptT $ wxUserPayParseRefundQueryItem x resp_params) [0 .. n-1]

             _              -> return []

  return $ WxUserPayRefundQueryResult
            m_dev_info
            trans_id
            out_trade_no
            total_fee
            settlement_total_fee
            cash_fee
            refund_account
            items
  where
    req_param = ExceptT . reqXmlTextField resp_params
-- }}}1


-- | 有些接口返回时没有签名．
-- 大部分输出输入报文都有签名
-- 目前未看到有第三种情况
data WxCallSignReq = WxCallSignReqInOut WxPayAppKey
                   | WxCallSignReqOut WxPayAppKey

wxCallSignReqOutAppKey :: WxCallSignReq -> WxPayAppKey
wxCallSignReqOutAppKey (WxCallSignReqInOut x) = x
wxCallSignReqOutAppKey (WxCallSignReqOut x)   = x

wxCallSignReqInAppKey :: WxCallSignReq -> Maybe WxPayAppKey
wxCallSignReqInAppKey (WxCallSignReqInOut x) = Just x
wxCallSignReqInAppKey (WxCallSignReqOut {})  = Nothing


wxPayCallInternal :: (WxppApiMonad env m)
                  => WxCallSignReq
                  -> String
                  -> WxPayParams
                  -> m ( Either WxPayCallError WxPayParams
                       , (LB.ByteString, LB.ByteString)
                       )
                  -- ^ 同时提供调用请求所用的报文及收到的报文的原始内容，以便分析问题
-- {{{1
wxPayCallInternal sign_req url params = do
  sess <- asks getWreqSession
  nonce <- wxppMakeNonce 32
  let doc_txt = wxPayRenderOutgoingXmlDoc (wxCallSignReqOutAppKey sign_req) params nonce
      req_lbs = encodeUtf8 doc_txt

  r <- liftIO (WS.post sess url req_lbs)
  let resp_lbs = r ^. responseBody

  fmap (, (req_lbs, resp_lbs)) $ wxPayParseInputXmlLbs (wxCallSignReqInAppKey sign_req) resp_lbs
-- }}}1


wxPayCallInternalHelper :: (WxppApiMonad env m)
                        => WxCallSignReq
                        -> String
                        -> WxPayParams
                        -> (WxPayParams -> m (Either WxPayCallError a))
                        -> m ( Either WxPayCallError a
                             , (LB.ByteString, LB.ByteString)
                             )
                        -- ^ 同时提供调用请求所用的报文及收到的报文的原始内容，以便分析问题
-- {{{1
wxPayCallInternalHelper sign_req url params f = do
  (err_or_x, io_lbs) <- wxPayCallInternal sign_req url params

  fmap (,(io_lbs)) $ case err_or_x of
                       Left err -> return $ Left err
                       Right x -> f x

-- }}}1


wxPayParseInputXmlLbs :: (MonadLogger m)
                      => Maybe WxPayAppKey
                      -> LB.ByteString
                      -> m (Either WxPayCallError WxPayParams)
-- {{{1
wxPayParseInputXmlLbs m_app_key lbs = runExceptT $ do
  case parseLBS def lbs of
      Left ex         -> do
        $logErrorS wxppLogSource $ "Failed to parse XML: " <> tshow ex
        throwError $ WxPayCallErrorXml ex

      Right resp_doc  -> do
        case wxPayParseIncomingXmlDoc m_app_key resp_doc of
          Left err -> do
            $logErrorS wxppLogSource $ "Invalid response XML: " <> err
            throwError $ WxPayCallErrorDiag $ WxPayDiagError err

          Right (Left err) -> do
            throwError err

          Right (Right resp_params) -> do
            return resp_params
-- }}}1


-- | 文档里的示示例, 时分秒的分隔符是全角的
-- 这个函数能兼容全角和半角两种情况
wxPayMchTransParseTimeStr :: String -> Maybe LocalTime
-- {{{1
wxPayMchTransParseTimeStr t =
  parseTimeM True locale fmt1 t <|> parseTimeM True locale fmt2 t
  where
    fmt1   = "%Y-%m-%d %H:%M:%S"
    fmt2   = "%Y-%m-%d %H：%M：%S"
    locale = defaultTimeLocale
-- }}}1


-- | 用户支付接口的时间格式的解释
wxUserPayParseTimeStr :: String -> Maybe LocalTime
-- {{{1
wxUserPayParseTimeStr t =
  parseTimeM True locale fmt1 t
  where
    fmt1   = "%Y%m%d%H%M%S"
    locale = defaultTimeLocale
-- }}}1


wxPayUserPayRenderTime :: FormatTime t => t -> String
-- {{{1
wxPayUserPayRenderTime = formatTime locale fmt1
  where
    fmt1   = "%Y%m%d%H%M%S"
    locale = defaultTimeLocale
-- }}}1


reqXmlTextField :: Monad m => WxPayParams -> Text -> m (Either WxPayDiagError Text)
reqXmlTextField vars n = runExceptT $
                      maybe
                        (throwError $ WxPayDiagError $ "Invalid response XML: Element '" <> n <> "' not found")
                        return
                        (lookup n vars)


reqXmlFeeField :: Monad m => WxPayParams -> Text -> m (Either WxPayDiagError WxPayMoneyAmount)
-- {{{1
reqXmlFeeField vars n = runExceptT $ do
  amount_t <- ExceptT $ reqXmlTextField vars n
  fmap WxPayMoneyAmount $
    maybe
      (throwError $ WxPayDiagError $ n <> "is not an integer: " <> amount_t)
      return
      (readMay amount_t)
-- }}}1


optXmlFeeField :: Monad m => WxPayParams -> Text -> m (Either WxPayDiagError (Maybe WxPayMoneyAmount))
-- {{{1
optXmlFeeField vars n = runExceptT $ runMaybeT $ do
  amount_t <- MaybeT $ return $ lookup n vars
  fmap WxPayMoneyAmount $
    maybe
      (throwError $ WxPayDiagError $ n <> "is not an integer: " <> amount_t)
      return
      (readMay amount_t)
-- }}}1


reqXmlTimeField :: Monad m
                => WxPayParams
                -> (String -> Maybe LocalTime)
                -> Text
                -> m (Either WxPayDiagError UTCTime)
-- {{{1
reqXmlTimeField vars parse_time n = runExceptT $ do
  time_t <- ExceptT $ reqXmlTextField vars n
  fmap (localTimeToUTC tz) $
    maybe
      (throwError $ WxPayDiagError $ "Invalid response XML: time string is invalid: " <> time_t)
      return
      (parse_time $ unpack time_t)
  where
    tz = hoursToTimeZone 8
-- }}}1


optXmlTimeField :: Monad m
                => WxPayParams
                -> (String -> Maybe LocalTime)
                -> Text
                -> m (Either WxPayDiagError (Maybe UTCTime))
-- {{{1
optXmlTimeField vars parse_time n = runExceptT $ runMaybeT $ do
  time_t <- MaybeT $ return $ lookup n vars
  fmap (localTimeToUTC tz) $
    maybe
      (throwError $ WxPayDiagError $ "Invalid response XML: time string is invalid: " <> time_t)
      return
      (parse_time $ unpack time_t)
  where
    tz = hoursToTimeZone 8
-- }}}1


-- vim: set foldmethod=marker:
