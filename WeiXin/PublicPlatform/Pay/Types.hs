{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Pay.Types where

import ClassyPrelude

-- {{{1 imports
import           Control.DeepSeq        (NFData)
import           Data.Binary            (Binary)
import           Data.Aeson             (ToJSON(..), object, (.=))
import           Database.Persist.Sql   (PersistField (..), PersistFieldSql (..))
import           Text.Blaze.Html        (ToMarkup (..))
import           Text.Shakespeare.I18N  (ToMessage (..))

import           Text.Parsec.TX.Utils   (SimpleStringRep (..), deriveJsonS,
                                         derivePersistFieldS,
                                         makeSimpleParserByTable)
import           Yesod.Helpers.Parsec   (derivePathPieceS)

import WeiXin.PublicPlatform.Types
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


-- | 查询或支付成功通知接口中核心提供的数据
{--
data WxPayUserPayOkInfo = WxPayUserPayOkInfo
  { wxPayUserPayDeviceInfo :: Maybe WxPayDeviceInfo
  , wxPayUserPayOpenId :: WxppOpenID
  , wxPayUserPayIsSubs :: Maybe Bool
  -- ^ 用户是否关注公众号
  , wxPayUserPayTradeType :: WxPayTradeType
  , 
  }
--}


-- vim: set foldmethod=marker:
