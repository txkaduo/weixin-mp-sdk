module WeiXin.PublicPlatform.Pay.Function where

import ClassyPrelude

-- {{{1 imports
import           Control.Lens           hiding ((.=))
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader   (asks)
import           Control.Monad.Trans.Maybe
import qualified Crypto.Hash.MD5        as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import           Data.Default           (def)
import           Data.Monoid            (Endo (..))
import           Data.Aeson             (object, (.=), encode)
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

import           Text.Parsec.TX.Utils   (SimpleStringRep (..), parseMaybeSimpleEncoded)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Security
import WeiXin.PublicPlatform.Pay.Types
-- }}}1


-- | 微信签名算法
wxPaySign :: WxPayAppKey
          -> WxPayParams
          -- ^ not including: nonce_str, key
          -> Nonce
          -> WxPaySignature
wxPaySign (WxPayAppKey ak) params (Nonce nonce_str) =
-- {{{1
  WxPaySignature $ toUpper $ fromString $
    C8.unpack $ B16.encode $ MD5.hash $ encodeUtf8 str_to_sign
  where
    params_all  = insertMap "nonce_str" nonce_str $ params
    mks k v     = mconcat [ k, "=", v ]
    str_to_sign = intercalate "&" $
                    fmap (uncurry mks) $
                      filter (not . null . snd) $
                        (sortBy (comparing fst) (mapToList params_all)) <> [("key", ak)]
-- }}}1


-- | 微信支付调用XML
wxPayOutgoingXmlDoc :: WxPayAppKey
                    -> WxPayParams
                    -- ^ not including: nonce_str, key
                    -> Nonce
                    -> Document
wxPayOutgoingXmlDoc app_key params nonce@(Nonce raw_nonce) =
-- {{{1
  Document (Prologue [] Nothing []) root []
  where
    root        = Element "xml" mempty nodes
    nodes       = map (uncurry mk_node) (mapToList params) <> [ node_nonce, node_sign ]
    node_nonce  = mk_node "nonce_str" raw_nonce
    sign        = wxPaySign app_key params nonce
    node_sign   = mk_node "sign" (unWxPaySignature sign)
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


wxPayParseIncomingXmlDoc :: WxPayAppKey
                        -> Document
                        -> Either Text WxPayParams
wxPayParseIncomingXmlDoc app_key doc = do
-- {{{1
  (nonce, params1) <- fmap (first Nonce) $ pop_up_find "nonce_str" all_params
  (sign, params2) <- fmap (first WxPaySignature) $ pop_up_find "sign" params1
  let params = params2
  let sign2 = wxPaySign app_key params nonce

  unless (sign2 == sign) $ do
    Left $ "incorrect signature"

  return params

  where
    cursor = fromDocument doc

    all_params = mapFromList $
                  catMaybes $ map param_from_node $
                    cursor $| child &| node

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
                -> m (Either WxPayCallResultError WxPayPrepayOk)
wxUserPayPrepay common_params notify_url amount out_trade_no trade_type ip_str body details m_prod_id m_open_id m_attach = do
-- {{{1
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

  runExceptT $ do
    resp_params <- ExceptT $ wxPayCallInternal app_key url params
    let req_param = reqXmlTextField resp_params

    prepay_id <- fmap WxUserPayPrepayId $ req_param "prepay_id"
    trade_type_t <- req_param "trade_type"
    r_trade_type <- case parseMaybeSimpleEncoded trade_type_t of
                    Nothing -> throwM $ WxPayDiagError $ "Unknown trade_type: " <> trade_type_t
                    Just x  -> return x

    let m_code_url = fmap UrlText $ lookup "code_url" resp_params

    return $ WxPayPrepayOk prepay_id r_trade_type m_code_url

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
                 -> m (Either WxPayCallResultError WxPayTransOk)
wxPayMchTransfer common_params m_dev_info mch_trade_no open_id check_name pay_amount desc ip_str = do
-- {{{1
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
                        WxNoCheckName -> Nothing

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


  runExceptT $ do
    resp_params <- ExceptT $ wxPayCallInternal app_key url params
    let req_param = reqXmlTextField resp_params

    mch_out_trade_no <- fmap WxMchTransMchNo $ req_param "partner_trade_no"
    unless (mch_out_trade_no == mch_trade_no) $ do
      throwM $ WxPayDiagError $
                "Unexpected response data: partner_trade_no is not the same as input: "
                <> tshow mch_out_trade_no

    wx_trade_no <- fmap WxMchTransWxNo $ req_param "payment_no"
    pay_time <- reqXmlTimeField resp_params wxPayMchTransParseTimeStr "payment_time"

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
                     -> m (Either WxPayCallResultError WxPayMchTransInfo)
wxPayMchTransferInfo common_params mch_trade_no = do
-- {{{1
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfMmPayApiBase url_conf <> "/gettransferinfo"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $ catMaybes
                    [ Just $ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Just $ Endo $ insertMap "appid" (unWxppAppID app_id)
                    , Just $ Endo $ insertMap "partner_trade_no" (unWxMchTransMchNo mch_trade_no)
                    ])

  runExceptT $ do
    resp_params <- ExceptT $ wxPayCallInternal app_key url params
    let req_param = reqXmlTextField resp_params

    mch_out_trade_no <- fmap WxMchTransMchNo $ req_param "partner_trade_no"
    unless (mch_out_trade_no == mch_trade_no) $ do
      throwM $ WxPayDiagError $
                "Unexpected response data: partner_trade_no is not the same as input: "
                <> tshow mch_out_trade_no

    -- 关于 detai_id 的意义不明，暂时认为这就是之前的 payment_no
    wx_trade_no <- fmap WxMchTransWxNo $ req_param "detail_id"

    st <- req_param "status"
    status <- case st of
                "SUCCESS"     -> return WxMmTransStatusSccess
                "PROCESSING"  -> return WxMmTransStatusProcessing
                "FAILED"      -> WxMmTransStatusFailed <$> req_param "reason"
                _             -> throwM $ WxPayDiagError $ "status is recognized: " <> st

    open_id <- WxppOpenID <$> req_param "openid"
    let m_recv_name = lookup "transfer_name" resp_params

    amount <- reqXmlFeeField resp_params "payment_amount"

    trans_time <- reqXmlTimeField resp_params wxPayMchTransParseTimeStr "transfer_time"

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
-- }}}1



wxPayCallInternal :: (WxppApiMonad env m)
                  => WxPayAppKey
                  -> String
                  -> WxPayParams
                  -> m (Either WxPayCallResultError WxPayParams)
wxPayCallInternal app_key url params = do
-- {{{1
  sess <- asks getWreqSession
  nonce <- wxppMakeNonce 32
  let doc_txt = wxPayRenderOutgoingXmlDoc app_key params nonce

  r <- liftIO (WS.post sess url $ encodeUtf8 doc_txt)
  let lbs = r ^. responseBody
  case parseLBS def lbs of
      Left ex         -> do
        $logErrorS wxppLogSource $ "Failed to parse XML: " <> tshow ex
        throwM ex

      Right resp_doc  -> do
        case wxPayParseIncomingXmlDoc app_key resp_doc of
          Left err -> do
            $logErrorS wxppLogSource $ "Invalid response XML: " <> err
            throwM $ WxPayDiagError err

          Right resp_params -> do
            let req_param n = maybe
                                  (throwM $ WxPayDiagError $ "Invalid response XML: Element '" <> n <> "' not found")
                                  return
                                  (lookup n resp_params)

            ret_code <- req_param "return_code"
            unless (ret_code == "SUCCESS") $ do
              let m_err_msg = lookup "return_msg" resp_params
              throwM $ WxPayCallReturnError m_err_msg

            result_code <- req_param "result_code"
            if result_code == "SUCCESS"
               then do
                 return $ Right resp_params

               else do
                 -- failed
                 err_code <- fmap WxPayErrorCode $ req_param "err_code"
                 err_desc <- req_param "err_code_des"
                 return $ Left $ WxPayCallResultError err_code err_desc
-- }}}1


-- | 文档里的示示例, 时分秒的分隔符是全角的
-- 这个函数能兼容全角和半角两种情况
wxPayMchTransParseTimeStr :: String -> Maybe LocalTime
wxPayMchTransParseTimeStr t =
-- {{{1
  parseTimeM True locale fmt1 t <|> parseTimeM True locale fmt2 t
  where
    fmt1   = "%Y-%m-%d %H:%M:%S"
    fmt2   = "%Y-%m-%d %H：%M：%S"
    locale = defaultTimeLocale
-- }}}1


-- | 用户支付接口的时间格式的解释
wxUserPayParseTimeStr :: String -> Maybe LocalTime
wxUserPayParseTimeStr t =
-- {{{1
  parseTimeM True locale fmt1 t
  where
    fmt1   = "%Y%m%d%H%M%S"
    locale = defaultTimeLocale
-- }}}1


wxPayUserPayRenderTime :: FormatTime t => t -> String
wxPayUserPayRenderTime = formatTime locale fmt1
-- {{{1
  where
    fmt1   = "%Y%m%d%H%M%S"
    locale = defaultTimeLocale
-- }}}1


reqXmlTextField :: MonadThrow m => WxPayParams -> Text -> m Text
reqXmlTextField vars n = maybe
                        (throwM $ WxPayDiagError $ "Invalid response XML: Element '" <> n <> "' not found")
                        return
                        (lookup n vars)


reqXmlFeeField :: MonadThrow m => WxPayParams -> Text -> m WxPayMoneyAmount
reqXmlFeeField vars n = do
  amount_t <- reqXmlTextField vars n
  fmap WxPayMoneyAmount $
    maybe
      (throwM $ WxPayDiagError $ n <> "is not an integer: " <> amount_t)
      return
      (readMay amount_t)


optXmlFeeField :: MonadThrow m => WxPayParams -> Text -> m (Maybe WxPayMoneyAmount)
optXmlFeeField vars n = runMaybeT $ do
  amount_t <- MaybeT $ return $ lookup n vars
  fmap WxPayMoneyAmount $
    maybe
      (throwM $ WxPayDiagError $ n <> "is not an integer: " <> amount_t)
      return
      (readMay amount_t)


reqXmlTimeField :: MonadThrow m
                => WxPayParams
                -> (String -> Maybe LocalTime)
                -> Text
                -> m UTCTime
reqXmlTimeField vars parse_time n = do
  time_t <- reqXmlTextField vars n
  fmap (localTimeToUTC tz) $
    maybe
      (throwM $ WxPayDiagError $ "Invalid response XML: time string is invalid: " <> time_t)
      return
      (parse_time $ unpack time_t)
  where
    tz = hoursToTimeZone 8

-- vim: set foldmethod=marker:
