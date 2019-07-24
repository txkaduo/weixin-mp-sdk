{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Pay.Types where

-- {{{1 imports
import           ClassyPrelude

import           Control.DeepSeq        (NFData)
import           Data.Binary            (Binary)
import           Data.Aeson             (FromJSON(..), ToJSON(..), object, (.=), withObject, (.:))
import           Data.Default           (Default(..))
import           Database.Persist.Sql   (PersistField (..), PersistFieldSql (..))
import           Text.Blaze.Html        (ToMarkup (..))
import           Text.Shakespeare.I18N  (ToMessage (..))

import           Text.Parsec.TX.Utils   ( SimpleEncode(..)
                                        , deriveJsonS, deriveSimpleStringRepEnumBounded, derivePersistFieldS
                                        )
import           Yesod.Helpers.Parsec   (derivePathPieceS)

import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Utils
import WeiXin.PublicPlatform.Pay.BankCode
-- }}}1


-- | 微信支付：预支付交易会话标识
newtype WxUserPayPrepayId = WxUserPayPrepayId { unWxUserPayPrepayId :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


-- | 微信支付：商户订单号
newtype WxUserPayOutTradeNo = WxUserPayOutTradeNo { unWxUserPayOutTradeNo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


-- | 体现某个类型包含了已生成好的商户订单
class HaveWxUserPayOutTradeNo a where
  getWxUserPayOutTradeNo :: a -> WxUserPayOutTradeNo


-- | 微信支付：商户订单号
newtype WxUserPayOutRefundNo = WxUserPayOutRefundNo { unWxUserPayOutRefundNo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , FromJSON, ToJSON
           , NFData
           , ToMessage, ToMarkup)


-- | 多个接口要求输入一个ip参数
newtype WxPayParamIpStr = WxPayParamIpStr { unWxPayParamIpStr :: Text }
  deriving (Show, Read, Eq, Ord, PersistFieldSql, PersistField)


-- | 商户自定义的商品ID
newtype WxPayProductId = WxPayProductId { unWxPayProductId :: Text }
  deriving (Show, Eq, Ord)


-- | 微信支付订单号
newtype WxUserPayTransId = WxUserPayTransId { unWxUserPayTransId :: Text }
  deriving (Show, Eq, Ord
           , PersistFieldSql, PersistField
           )


-- | 用户支付: 退款单号
newtype WxUserPayRefundId = WxUserPayRefundId { unWxUserPayRefundId :: Text }
  deriving (Show, Eq, Ord
           , PersistFieldSql, PersistField
           , FromJSON, ToJSON
           )


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
-- {{{1 instances
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , ToJSON
           , NFData
           , ToMessage, ToMarkup)

instance FromJSON WxPayAppKey where
  parseJSON = fmap WxPayAppKey
                . (parseJSON >=> nonEmptyJsonText "WxPayAppKey id cannot be empty text")
-- }}}1

newtype WxPaySignature = WxPaySignature { unWxPaySignature :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , NFData
           , ToMessage, ToMarkup)


-- | 微信支付商户号
newtype WxPayMchID = WxPayMchID { unWxPayMchID :: Text }
-- {{{1 instances
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , ToJSON
           , NFData
           , ToMessage, ToMarkup)

instance FromJSON WxPayMchID where
  parseJSON = fmap WxPayMchID
                . (parseJSON >=> nonEmptyJsonText "WxPayMchID id cannot be empty text")
-- }}}1


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
wxPayMoneyAmountFromYuanEither :: (Show a, RealFrac a, IsString s)
                               => a
                               -> Either s WxPayMoneyAmount
wxPayMoneyAmountFromYuanEither y =
  if abs (fromIntegral fen_int - fen) > 0.001
     then Left $ fromString $
                  "Cannot convert to convert to WxPayMoneyAmount loselessly: " <> show y
                  <> "because " <> show fen_int <> " != " <> show fen
     else Right $ WxPayMoneyAmount fen_int
  where
    fen = y * fromIntegral (100 :: Int)
    fen_int = round fen

wxPayMoneyAmountFromYuan :: (Show a, RealFrac a)
                         => a
                         -> WxPayMoneyAmount
wxPayMoneyAmountFromYuan y =
  either error id $ wxPayMoneyAmountFromYuanEither y


wxPayMoneyAmountToYuan :: Integral a => WxPayMoneyAmount -> a
wxPayMoneyAmountToYuan = fromIntegral . unWxPayMoneyAmount


-- | 对应文档 goods_detail 数组里的一个元素
data WxPayGoodsDetail = WxPayGoodsDetail
  { wxPayGoodsIdStr     :: Text
  -- ^ 大小写字母，数字，中划线，下划线组成，最长32
  , wxPayGoodsName      :: Maybe Text
  , wxPayGoodsWxId      :: Maybe Text
  -- ^ 微信支付定义的统一商品编号
  , wxPayGoodsNum       :: Int
  , wxPayGoodsUnitPrice :: WxPayMoneyAmount
  }
  deriving (Show)

-- {{{1
instance ToJSON WxPayGoodsDetail where
  toJSON x = object $ catMaybes
              [ Just $ "goods_id" .= wxPayGoodsIdStr x
              , ("wxpay_goods_id" .=) <$> wxPayGoodsWxId x
              , ("goods_name" .=) <$> wxPayGoodsName x
              , Just $ "quantity" .= wxPayGoodsNum x
              , Just $ "price" .= unWxPayMoneyAmount (wxPayGoodsUnitPrice x)
              ]
-- }}}1


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
$(deriveSimpleStringRepEnumBounded "WxPayTradeType")

instance SimpleEncode WxPayTradeType where
  simpleEncode mtype =
      case mtype of
          WxPayTradeJsApi    -> "JSAPI"
          WxPayTradeNative   -> "NATIVE"
          WxPayTradeApp      -> "APP"
          WxPayTradeMicroPay -> "MICROPAY"
-- }}}1


-- | 代金券类型
data WxPayCouponType = WxPayCouponCash
                     | WxPayCouponNonCash
                     deriving (Show, Eq, Ord, Enum, Bounded)


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
$(deriveSimpleStringRepEnumBounded "WxPayResultCode")

instance SimpleEncode WxPayResultCode where
  simpleEncode WxPaySuccess = "SUCCESS"
  simpleEncode WxPayFail    = "FAIL"
-- }}}1


-- | 微信支付错误代码
newtype WxPayErrorCode = WxPayErrorCode { unWxPayErrorCode :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , PersistFieldSql, PersistField
           , NFData
           , ToMessage, ToMarkup)


type WxPayParams = HashMap Text Text

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


-- | 报文解释错误
data WxPayDiagError = WxPayDiagError Text
                      deriving (Show)

instance Exception WxPayDiagError


-- | 业务层面上的错误: result_code 为 FAIL 时的数据
data WxPayCallResultError = WxPayCallResultError
                              WxPayErrorCode
                              Text
                            deriving (Show)


-- | 微信支付接口调用的各种出错情况
-- 未包括网络IO等的底层错误
data WxPayCallError = WxPayCallErrorResult WxPayCallResultError
                    | WxPayCallErrorReturn WxPayCallReturnError
                    | WxPayCallErrorDiag WxPayDiagError
                    | WxPayCallErrorXml SomeException
                    deriving (Show)



-- | 统一下单接口成功时的返回报文
data WxPayPrepayOk = WxPayPrepayOk
  { wxPayPrepayId        :: WxUserPayPrepayId
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

-- | 用户支付状态
-- 仅出现在查询接口的返回报文内
data WxUserPayStatus = WxUserPaySuccess
                     | WxUserPayRefund
                     | WxUserPayNotPay
                     | WxUserPayClosed
                     | WxUserPayRevoked
                     | WxUserPayUserPaying
                     | WxUserPayPayError
                     deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
$(derivePersistFieldS "WxUserPayStatus")
$(derivePathPieceS "WxUserPayStatus")
$(deriveJsonS "WxUserPayStatus")
$(deriveSimpleStringRepEnumBounded "WxUserPayStatus")

instance SimpleEncode WxUserPayStatus where
  simpleEncode WxUserPaySuccess    = "success"
  simpleEncode WxUserPayRefund     = "refund"
  simpleEncode WxUserPayNotPay     = "not_pay"
  simpleEncode WxUserPayClosed     = "closed"
  simpleEncode WxUserPayRevoked    = "revoked"
  simpleEncode WxUserPayUserPaying = "paying"
  simpleEncode WxUserPayPayError   = "error"
-- }}}1

-- | 企业支付转账状态
data WxMmTransStatus = WxMmTransStatusSccess
                     | WxMmTransStatusFailed Text  -- ^ 失败及其原因
                     | WxMmTransStatusProcessing
                     deriving (Show)


-- | 微信企业支付的查询接口成功时的有效返回内容
data WxPayMchTransInfo = WxPayMchTransInfo
  { wxPayMchTransInfoMchNo      :: WxMchTransMchNo
  , wxPayMchTransInfoWxNo       :: WxMchTransWxNo
  -- ^ detail_id
  , wxPayMchTransInfoStatus     :: WxMmTransStatus
  , wxPayMchTransInfoOpenID     :: WxppOpenID
  , wxPayMchTransInfoRecvName   :: Maybe Text
  , wxPayMchTransInfoAmount     :: WxPayMoneyAmount
  , wxPayMchTransInfoTransTime  :: UTCTime
  , wxPayMchTransInfoTransDesc  :: Text
  }
  deriving (Show)


-- | 支付成功通知接口中核心提供的数据
-- 这个信息跟 WxUserPayQueryInfo 内容，当支付是成功时是重合的
data WxUserPaySuccInfo = WxUserPaySuccInfo
-- {{{1
  { wxUserPaySuccDeviceInfo         :: Maybe WxPayDeviceInfo
  , wxUserPaySuccOpenId             :: WxppOpenID
  , wxUserPaySuccIsSubs             :: Maybe Bool
  -- ^ 用户是否关注公众号
  , wxUserPaySuccTradeType          :: WxPayTradeType
  -- , wxUserPaySuccStatus             :: WxUserPayStatus
  , wxUserPaySuccBankCode           :: Either Text BankCode
  -- ^ Left 是无法识别的银行代码．运行表明，微信会发不在文档里的银行代码
  , wxUserPaySuccTotalFee           :: WxPayMoneyAmount
  , wxUserPaySuccSettlementTotalFee :: Maybe WxPayMoneyAmount
  , wxUserPaySuccCashFee            :: Maybe WxPayMoneyAmount
  -- ^ 现金支付金额：文档说是必须出现的字段，但在两个示例中都没出现这个字段
  -- 估计这应该是可选的
  , wxUserPaySuccCouponFee          :: Maybe WxPayMoneyAmount
  , wxUserPaySuccTransId            :: WxUserPayTransId
  , wxUserPaySuccOutTradeNo         :: WxUserPayOutTradeNo
  , wxUserPaySuccAttach             :: Maybe Text
  , wxUserPaySuccTimeEnd            :: UTCTime
  }
-- }}}1


-- | 查询接口提供的数据
-- 有很多字段文档说一定有，但实测，至少在若支付单未成功时，无此字段
data WxUserPayQueryInfo = WxUserPayQueryInfo
-- {{{1
  { wxUserPayQueryDeviceInfo         :: Maybe WxPayDeviceInfo
  , wxUserPayQueryOpenId             :: Maybe WxppOpenID
  , wxUserPayQueryIsSubs             :: Maybe Bool
  -- ^ 用户是否关注公众号
  , wxUserPayQueryTradeType          :: Maybe WxPayTradeType
  -- , wxUserPayQueryStatus             :: WxUserPayStatus
  , wxUserPayQueryBankCode           :: Maybe (Either Text BankCode)
  -- ^ Left 是无法识别的银行代码．运行表明，微信会发不在文档里的银行代码
  , wxUserPayQueryTotalFee           :: Maybe WxPayMoneyAmount
  , wxUserPayQuerySettlementTotalFee :: Maybe WxPayMoneyAmount
  , wxUserPayQueryCashFee            :: Maybe WxPayMoneyAmount
  -- ^ 现金支付金额：文档说是必须出现的字段，但在两个示例中都没出现这个字段
  -- 估计这应该是可选的
  , wxUserPayQueryCouponFee          :: Maybe WxPayMoneyAmount
  , wxUserPayQueryTransId            :: Maybe WxUserPayTransId
  , wxUserPayQueryOutTradeNo         :: WxUserPayOutTradeNo
  , wxUserPayQueryAttach             :: Maybe Text
  , wxUserPayQueryTimeEnd            :: Maybe UTCTime
  }
-- }}}1


data WxPayRefundChannel = WxPayRefundOriginal
                        | WxPayRefundBalance
                        deriving (Show, Eq, Ord, Enum, Bounded)


data WxPayRefundStatus = WxPayRefundSuccess
                       | WxPayRefundFail
                       | WxPayRefundProcessing
                       | WxPayRefundChange
                       deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
$(derivePersistFieldS "WxPayRefundStatus")
$(derivePathPieceS "WxPayRefundStatus")
$(deriveJsonS "WxPayRefundStatus")
$(deriveSimpleStringRepEnumBounded "WxPayRefundStatus")

instance SimpleEncode WxPayRefundStatus where
  simpleEncode WxPayRefundSuccess    = "success"
  simpleEncode WxPayRefundFail       = "fail"
  simpleEncode WxPayRefundProcessing = "processing"
  simpleEncode WxPayRefundChange     = "change"
-- }}}1


data WxPayRefundAccount = WxPayRefundAccountUnsettledFunds
                        | WxPayRefundAccountRechargeFunds
                       deriving (Show, Eq, Ord, Enum, Bounded)

-- {{{1 instances
$(derivePersistFieldS "WxPayRefundAccount")
$(derivePathPieceS "WxPayRefundAccount")
$(deriveJsonS "WxPayRefundAccount")
$(deriveSimpleStringRepEnumBounded "WxPayRefundAccount")

instance SimpleEncode WxPayRefundAccount where
  simpleEncode WxPayRefundAccountUnsettledFunds = "unseltted"
  simpleEncode WxPayRefundAccountRechargeFunds  = "recharge"
-- }}}1


-- | 退款接口的各种可选参数
data WxPayRefundOptArgs = WxPayRefundOptArgs
  { wxPayRefundOptArgAccount :: Maybe WxPayRefundAccount
  , wxPayRefundOptArgChannel :: Maybe WxPayRefundChannel
  }

-- {{{1
instance Default WxPayRefundOptArgs where
  def = WxPayRefundOptArgs def def
-- }}}1


-- | 申请退款成功产生的返回
-- 查询退款接口也会提供这部分信息
data WxUserPayRefundReqResult = WxUserPayRefundReqResult
  { wxUserPayRefundReqReDeviceInfo          :: Maybe WxPayDeviceInfo
  , wxUserPayRefundReqReTransId             :: WxUserPayTransId
  , wxUserPayRefundReqReOutTradeNo          :: WxUserPayOutTradeNo
  , wxUserPayRefundReqReRefundId            :: WxUserPayRefundId
  , wxUserPayRefundReqReOutRefundNo         :: WxUserPayOutRefundNo
  , wxUserPayRefundReqReTotalFee            :: WxPayMoneyAmount
  , wxUserPayRefundReqReRefundFee           :: WxPayMoneyAmount
  , wxUserPayRefundReqReSettlementRefundFee :: Maybe WxPayMoneyAmount
  , wxUserPayRefundReqReCashFee             :: WxPayMoneyAmount
  , wxUserPayRefundReqReCashRefundFee       :: Maybe WxPayMoneyAmount
  , wxUserPayRefundReqReChannel             :: Maybe WxPayRefundChannel
  }


-- | 退款查询接口的返回内容
data WxUserPayRefundQueryResult = WxUserPayRefundQueryResult
  { wxUserPayRefundQueryReDeviceInfo         :: Maybe WxPayDeviceInfo
  , wxUserPayRefundQueryReTransId            :: WxUserPayTransId
  , wxUserPayRefundQueryReOutTradeNo         :: WxUserPayOutTradeNo
  , wxUserPayRefundQueryReTotalFee           :: WxPayMoneyAmount
  , wxUserPayRefundQueryReSettlementTotalFee :: Maybe WxPayMoneyAmount
  , wxUserPayRefundQueryReCashFee            :: WxPayMoneyAmount
  , wxUserPayRefundQueryReRefundAccount      :: Maybe WxPayRefundAccount
  , wxUserPayRefundQueryReItems              :: [WxUserPayRefundQueryItem]
  }


data WxUserPayRefundQueryItem = WxUserPayRefundQueryItem
  { wxUserPayRefundQueryItemOutRefundNo         :: WxUserPayOutRefundNo
  , wxUserPayRefundQueryItemRefundId            :: WxUserPayRefundId
  , wxUserPayRefundQueryItemStatus              :: WxPayRefundStatus
  , wxUserPayRefundQueryItemRefundChannel       :: Maybe WxPayRefundChannel
  , wxUserPayRefundQueryItemRefundFee           :: WxPayMoneyAmount
  , wxUserPayRefundQueryItemSettlementRefundFee :: Maybe WxPayMoneyAmount
  , wxUserPayRefundQueryItemRecvAccount         :: Text
  , wxUserPayRefundQueryItemCouponType          :: Maybe WxPayCouponType
  , wxUserPayRefundQueryItemCouponRefunds       :: [WxPayRefundQueryCouponRefundItem]
  }


data WxPayRefundQueryCouponRefundItem = WxPayRefundQueryCouponRefundItem
  { wxPayRefundQueryCouponRefundItemBatchId   :: Text
  , wxPayRefundQueryCouponRefundItemRefundId  :: Text
  , wxPayRefundQueryCouponRefundItemRefundFee :: WxPayMoneyAmount
  }


-- | 收集跟微信支付相关的配置选项
data WxPayConfig = WxPayConfig
    { wxPayConfigAppId       :: WxppAppID
    , wxPayConfigAppKey      :: WxPayAppKey
    , wxPayConfigMchId       :: WxPayMchID
    -- 以下两个字段仅用于生成 prepay 接口的 body 参数
    , wxPayConfigMchName     :: Text
    -- ^ 这个应该是公众号的名称
    , wxPayConfigMchCategory :: Text
    -- ^ 这个不知道能不能用固定的什么值
    }

-- {{{1 instances
instance FromJSON WxPayConfig where
  parseJSON = withObject "WxPayConfig" $ \ o ->
                WxPayConfig <$> o .: "app-id"
                            <*> o .: "app-key"
                            <*> o .: "mch-id"
                            <*> o .: "mch-name"
                            <*> o .: "mch-category"
-- }}}1

-- vim: set foldmethod=marker:
