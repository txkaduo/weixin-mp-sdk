{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Pay where

import ClassyPrelude

-- {{{1 imports
import           Control.DeepSeq        (NFData)
import           Control.Lens           hiding ((.=))
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader   (asks)
import qualified Crypto.Hash.MD5        as MD5
import           Data.Binary            (Binary)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import           Data.Default           (def)
import           Data.Monoid            (Endo (..))
import           Data.Aeson             (ToJSON(..), object, (.=), encode)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import           Data.Time              (LocalTime, hoursToTimeZone,
                                         localTimeToUTC)
import           Data.Time.Format       (parseTimeM)
import           Database.Persist.Sql   (PersistField (..), PersistFieldSql (..))
import           Network.Wreq           (responseBody)
import qualified Network.Wreq.Session   as WS
import           Text.Blaze.Html        (ToMarkup (..))
import           Text.Shakespeare.I18N  (ToMessage (..))
import           Text.XML               (Document (..), Element (..), Name (..),
                                         Node (..), Prologue (..), renderText, parseLBS)
import           Text.XML.Cursor        (child, content, fromDocument, fromNode
                                        , node, ($/), ($|), (&|)
                                        )

import           Text.Parsec.TX.Utils   (SimpleStringRep (..), deriveJsonS,
                                         derivePersistFieldS,
                                         makeSimpleParserByTable,
                                        parseMaybeSimpleEncoded)
import           Yesod.Helpers.Parsec   (derivePathPieceS)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.WS
import WeiXin.PublicPlatform.Security
-- }}}1


-- | 微信支付：预支付交易会话标识
newtype WxPayPrepayId = WxPayPrepayId { unWxPayPrepayId :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


-- | 微信支付：商户订单号
newtype WxPayOutTradeNo = WxPayOutTradeNo { unWxPayOutTradeNo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


-- | 体现某个类型包含了已生成好的商户订单
class HaveWxPayOutTradeNo a where
  getWxPayOutTradeNo :: a -> WxPayOutTradeNo


-- | 多个接口要求输入一个ip参数
newtype WxPayParamIpStr = WxPayParamIpStr { unWxPayParamIpStr :: Text }


-- | 商户自定义的商品ID
newtype WxPayProductId = WxPayProductId { unWxPayProductId :: Text }


-- | 微信企业支付所产生的订单号
newtype WxMchTransWxNo = WxMchTransWxNo { unWxMchTransWxNo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


-- | 企业支付的查询接口返回了个 detail_id 不知道是否就是前面的 WxMchTransWxNo
-- 如果不是一样的东西，就应该用新的类型对应
-- 但估计是一样的东西
{-
newtype WxMchTransDetailNo = WxMchTransDetailNo { unWxMchTransDetailNo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , NFData
           , ToMessage, ToMarkup)
--}


newtype WxPayAppKey = WxPayAppKey { unWxPayAppKey :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


newtype WxPaySignature = WxPaySignature { unWxPaySignature :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , NFData
           , ToMessage, ToMarkup)


-- | 微信支付商户号
newtype WxPayMchID = WxPayMchID { unWxPayMchID :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


-- | 微信支付的设备号
-- 至于 Info 这个词是因为文档也是用这个词的
newtype WxPayDeviceInfo = WxPayDeviceInfo { unWxPayDeviceInfo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)

webWxPayDeviceInfo :: WxPayDeviceInfo
webWxPayDeviceInfo = WxPayDeviceInfo "WEB"


-- | body 字段有格式要求，因此为它做一个专门的类型
data WxPayParamBody = WxPayParamBody Text Text
  deriving (Show)

renderWxPayParamBody :: WxPayParamBody -> Text
renderWxPayParamBody (WxPayParamBody x y) = x <> "-" <> y


-- | 金额指定用分作单位
newtype WxPayMoneyAmount = WxPayMoneyAmount { unWxPayMoneyAmount :: Int }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary, NFData)


-- | 从单位是元的数字转成 WxPayMoneyAmount
wxPayMoneyAmountFromYuanEither :: (Show a, Num a, RealFrac a, IsString s)
                               => a
                               -> Either s WxPayMoneyAmount
wxPayMoneyAmountFromYuanEither y =
  if fromIntegral fen_int /= fen
     then Left $ fromString $ "Cannot convert to convert to WxPayMoneyAmount loselessly: " <> show y
     else Right $ WxPayMoneyAmount fen_int
  where
    fen = y * fromIntegral (100 :: Int)
    fen_int = round fen

wxPayMoneyAmountFromYuan :: (Show a, Num a, RealFrac a)
                         => a
                         -> WxPayMoneyAmount
wxPayMoneyAmountFromYuan y =
  either error id $ wxPayMoneyAmountFromYuanEither y


wxPayMoneyAmountToYuan :: Integral a => WxPayMoneyAmount -> a
wxPayMoneyAmountToYuan = fromIntegral . unWxPayMoneyAmount


-- | 对应文档 goods_detail 数组里的一个元素
-- 但只保留必要的字段
-- 其它文档没解释清楚，又是可选的字段直接不反映在这里
data WxPayGoodsDetail = WxPayGoodsDetail
  { wxPayGoodsIdStr     :: Text
  , wxPayGoodsName      :: Text
  , wxPayGoodsNum       :: Int
  , wxPayGoodsUnitPrice :: WxPayMoneyAmount
  }
  deriving (Show)

instance ToJSON WxPayGoodsDetail where
  toJSON x = object
              [ "goods_id" .= wxPayGoodsIdStr x
              , "goods_name" .= wxPayGoodsName x
              , "goods_num" .= wxPayGoodsNum x
              , "price" .= unWxPayMoneyAmount (wxPayGoodsUnitPrice x)
              ]


-- | 交易类型
data WxPayTradeType = WxPayTradeJsApi
                    | WxPayTradeNative
                    | WxPayTradeApp
                    | WxPayTradeMicroPay  -- ^ 刷卡支付，不调用统一接口
                    deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
$(derivePersistFieldS "WxPayTradeType")
$(derivePathPieceS "WxPayTradeType")
$(deriveJsonS "WxPayTradeType")

instance SimpleStringRep WxPayTradeType where
    simpleEncode mtype =
        case mtype of
            WxPayTradeJsApi    -> "JSAPI"
            WxPayTradeNative   -> "NATIVE"
            WxPayTradeApp      -> "APP"
            WxPayTradeMicroPay -> "MICROPAY"

    simpleParser = makeSimpleParserByTable
                    [ ("JSAPI", WxPayTradeJsApi)
                    , ("NATIVE", WxPayTradeNative)
                    , ("APP", WxPayTradeApp)
                    , ("MICROPAY", WxPayTradeMicroPay)
                    ]
-- }}}1


-- | 微信支付接口的结果代码
-- 注意: 有两种结果代码: 返回状态状态码, 业务状态码
--       目前看, 内容是一致的
data WxPayResultCode = WxPaySuccess
                     | WxPayFail
                     deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
$(derivePersistFieldS "WxPayResultCode")
$(derivePathPieceS "WxPayResultCode")
$(deriveJsonS "WxPayResultCode")

instance SimpleStringRep WxPayResultCode where
  simpleEncode WxPaySuccess = "SUCCESS"
  simpleEncode WxPayFail    = "FAIL"

  simpleParser = makeSimpleParserByTable
                    [ ("SUCCESS", WxPaySuccess)
                    , ("FAIL", WxPayFail)
                    ]
-- }}}1


-- | 微信支付错误代码
newtype WxPayErrorCode = WxPayErrorCode { unWxPayErrorCode :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


type WxPayParams = HashMap Text Text

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


wxPayParseIncmingXmlDoc :: WxPayAppKey
                        -> Document
                        -> Either Text WxPayParams
wxPayParseIncmingXmlDoc app_key doc = do
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


data WxCheckName =  WxNoCheckName
                  | WxOptCheckName Text
                  | WxReqCheckName Text
                  deriving (Eq, Show)


-- | 调用时的"通信标识" 为 FAIL 时的数据
-- 出现这种错误时, 认为是程序错误, 直接抛出异常
data WxPayCallReturnError = WxPayCallReturnError
                              (Maybe Text)  -- ^ error message
                              deriving (Show)

instance Exception WxPayCallReturnError


data WxPayDiagError = WxPayDiagError Text
                      deriving (Show)

instance Exception WxPayDiagError


-- | 业务层面上的错误: result_code 为 FAIL 时的数据
data WxPayCallResultError = WxPayCallResultError
                              WxPayErrorCode
                              Text
                            deriving (Show)


-- | 统一下单接口成功时的返回报文
data WxPayPrepayOk = WxPayPrepayOk
  { wxPayPrepayId        :: WxPayPrepayId
  , wxPayPrepayTradeType :: WxPayTradeType
  , wxPayPrepayQrCodeUrl :: Maybe UrlText
  }

-- | 支付时的商户订单号
newtype WxMchTransMchNo = WxMchTransMchNo { unWxMchTransMchNo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


-- | 微信企业支付成功调用返回的结果
data WxPayTransOk = WxPayTransOk
  { wxPayTransOkPartnerTradeNo :: WxMchTransMchNo
  , wxPayTransOkWxTradeNo      :: WxMchTransWxNo
  , wxPayTransOkPaidTime       :: UTCTime
    -- ^ 这个时间的意义不明，不一定是真正成功的时间
    -- 因为还有一个查询接口，有可能返回＂处理中＂的状态
    -- 说明不是一次调用成功就能保证成功的
  }


-- | 支付接口基本上都要这些参数
data WxPayCommonParams = WxPayCommonParams
                          WxppAppID
                          WxPayAppKey
                          WxPayMchID

---------------------------------------------------------


-- | 统一下单接口
wxPayPrepay :: WxppApiMonad env m
            => WxPayCommonParams
            -> UrlText             -- ^ 通知地址
            -> WxPayMoneyAmount
            -> WxPayOutTradeNo     -- ^ 商户订单号
            -> WxPayTradeType      -- ^ 交易类型
            -> WxPayParamIpStr     -- ^ 终端IP
            -> WxPayParamBody
            -> [WxPayGoodsDetail]
            -> Maybe WxPayProductId
            -> Maybe WxppOpenID    -- ^ required, if trade_type == JSAPI
            -> Maybe Text          -- ^ 附加数据
            -> m (Either WxPayCallResultError WxPayPrepayOk)
wxPayPrepay common_params notify_url amount out_trade_no trade_type ip_str body details m_prod_id m_open_id m_attach = do
-- {{{1
  url_conf <- asks getWxppUrlConfig
  let url = wxppUrlConfUserPayApiBase url_conf <> "/unifiedorder"
  let params :: WxPayParams
      params = mempty &
                (appEndo $ mconcat $ catMaybes
                    [ Just $ Endo $ insertMap "mch_id" (unWxPayMchID mch_id)
                    , Just $ Endo $ insertMap "appid" (unWxppAppID app_id)
                    , Just $ Endo $ insertMap "out_trade_no" (unWxPayOutTradeNo out_trade_no)
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
    let lookup_param n = maybe
                          (throwM $ WxPayDiagError $ "Invalid response XML: Element '" <> n <> "' not found")
                          return
                          (lookup n resp_params)

    prepay_id <- fmap WxPayPrepayId $ lookup_param "prepay_id"
    trade_type_t <- lookup_param "trade_type"
    r_trade_type <- case parseMaybeSimpleEncoded trade_type_t of
                    Nothing -> throwM $ WxPayDiagError $ "Unknown trade_type" <> trade_type_t
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
                 => WxPayAppKey
                 -> WxPayMchID
                 -> Maybe WxPayDeviceInfo
                 -> WxMchTransMchNo
                 -> WxppAppID
                 -> WxppOpenID
                 -> WxCheckName
                 -> WxPayMoneyAmount
                 -> Text          -- ^ description
                 -> WxPayParamIpStr          -- ^ ip address
                 -> m (Either WxPayCallResultError WxPayTransOk)
wxPayMchTransfer app_key mch_id m_dev_info mch_trade_no app_id open_id check_name pay_amount desc ip_str = do
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
    let lookup_param n = maybe
                          (throwM $ WxPayDiagError $ "Invalid response XML: Element '" <> n <> "' not found")
                          return
                          (lookup n resp_params)

    mch_out_trade_no <- fmap WxMchTransMchNo $ lookup_param "partner_trade_no"
    unless (mch_out_trade_no == mch_trade_no) $ do
      throwM $ WxPayDiagError $
                "Unexpected response data: partner_trade_no is not the same as input: "
                <> tshow mch_out_trade_no

    wx_trade_no <- fmap WxMchTransWxNo $ lookup_param "payment_no"
    pay_time_t <- lookup_param "payment_time"
    local_time <- maybe
                    (throwM $ WxPayDiagError $ "Invalid response XML: time string is invalid: " <> pay_time_t)
                    return
                    (wxPayParseTimeStr $ T.unpack pay_time_t)

    let pay_time = localTimeToUTC tz local_time

    return $ WxPayTransOk mch_out_trade_no wx_trade_no pay_time

  where
    tz = hoursToTimeZone 8
-- }}}1


-- | 支付转账状态
data WxPayStatus = WxPayStatusSccess
                | WxPayStatusFailed Text  -- ^ 失败及其原因
                | WxPayStatusProcessing
                deriving (Show)


-- | 微信企业支付的查询接口成功时的有效返回内容
data WxPayMchTransInfo = WxPayMchTransInfo
  { wxPayMchTransInfoMchNo      :: WxMchTransMchNo
  , wxPayMchTransInfoWxNo       :: WxMchTransWxNo
  -- ^ detail_id
  , wxPayMchTransInfoStatus     :: WxPayStatus
  , wxPayMchTransInfoOpenID     :: WxppOpenID
  , wxPayMchTransInfoRecvName   :: Maybe Text
  , wxPayMchTransInfoAmount     :: WxPayMoneyAmount
  , wxPayMchTransInfoTransTime  :: UTCTime
  , wxPayMchTransInfoTransDesc  :: Text
  }
  deriving (Show)


-- | 微信企业支付结果查询
-- CAUTION: 目前未实现双向数字证书认证
--          实用上的解决方法是使用反向代理(例如HAProxy)提供双向证书认证,
--          我们这里只发起普通的http/https请求
wxPayMchTransferInfo :: (WxppApiMonad env m)
                     => WxPayAppKey
                     -> WxPayMchID
                     -> WxMchTransMchNo
                     -> WxppAppID
                     -> m (Either WxPayCallResultError WxPayMchTransInfo)
wxPayMchTransferInfo app_key mch_id mch_trade_no app_id = do
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
    let lookup_param n = maybe
                          (throwM $ WxPayDiagError $ "Invalid response XML: Element '" <> n <> "' not found")
                          return
                          (lookup n resp_params)

    mch_out_trade_no <- fmap WxMchTransMchNo $ lookup_param "partner_trade_no"
    unless (mch_out_trade_no == mch_trade_no) $ do
      throwM $ WxPayDiagError $
                "Unexpected response data: partner_trade_no is not the same as input: "
                <> tshow mch_out_trade_no

    -- 关于 detai_id 的意义不明，暂时认为这就是之前的 payment_no
    wx_trade_no <- fmap WxMchTransWxNo $ lookup_param "detail_id"

    st <- lookup_param "status"
    status <- case st of
                "SUCCESS"     -> return WxPayStatusSccess
                "PROCESSING"  -> return WxPayStatusProcessing
                "FAILED"      -> WxPayStatusFailed <$> lookup_param "reason"
                _             -> throwM $ WxPayDiagError $ "status is recognized: " <> st

    open_id <- WxppOpenID <$> lookup_param "openid"
    let m_recv_name = lookup "transfer_name" resp_params

    amount_t <- lookup_param "payment_amount"
    amount <- fmap WxPayMoneyAmount $
                maybe
                  (throwM $ WxPayDiagError $ "payment_amount is not an integer: " <> amount_t)
                  return
                  (readMay amount_t)

    trans_time_t <- lookup_param "transfer_time"
    local_time <- maybe
                    (throwM $ WxPayDiagError $ "Invalid response XML: time string is invalid: " <> trans_time_t)
                    return
                    (wxPayParseTimeStr $ T.unpack trans_time_t)

    let trans_time = localTimeToUTC tz local_time

    desc <- lookup_param "desc"

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
    tz = hoursToTimeZone 8
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
        case wxPayParseIncmingXmlDoc app_key resp_doc of
          Left err -> do
            $logErrorS wxppLogSource $ "Invalid response XML: " <> err
            throwM $ WxPayDiagError err

          Right resp_params -> do
            let lookup_param n = maybe
                                  (throwM $ WxPayDiagError $ "Invalid response XML: Element '" <> n <> "' not found")
                                  return
                                  (lookup n resp_params)

            ret_code <- lookup_param "return_code"
            unless (ret_code == "SUCCESS") $ do
              let m_err_msg = lookup "return_msg" resp_params
              throwM $ WxPayCallReturnError m_err_msg

            result_code <- lookup_param "result_code"
            if result_code == "SUCCESS"
               then do
                 return $ Right resp_params

               else do
                 -- failed
                 err_code <- fmap WxPayErrorCode $ lookup_param "err_code"
                 err_desc <- lookup_param "err_code_des"
                 return $ Left $ WxPayCallResultError err_code err_desc
-- }}}1


-- | 文档里的示示例, 时分秒的分隔符是全角的
-- 这个函数能兼容全角和半角两种情况
wxPayParseTimeStr :: String -> Maybe LocalTime
wxPayParseTimeStr t =
-- {{{1
  parseTimeM True locale fmt1 t <|> parseTimeM True locale fmt2 t
  where
    fmt1   = "%Y-%m-%d %H:%M:%S"
    fmt2   = "%Y-%m-%d %H：%M：%S"
    locale = defaultTimeLocale
-- }}}1


-- vim: set foldmethod=marker:
