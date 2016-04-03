{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WeiXin.PublicPlatform.Pay where

import ClassyPrelude

import           Control.DeepSeq        (NFData)
import qualified Crypto.Hash.MD5        as MD5
import           Data.Binary            (Binary)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import           Data.Default           (def)
import qualified Data.Text.Lazy         as LT
import           Text.Blaze.Html        (ToMarkup (..))
import           Text.Shakespeare.I18N  (ToMessage (..))
import           Text.XML               ( Document(..), Node(..), Element(..)
                                        , Name(..), Prologue(..), renderText
                                        )
import           Text.XML.Cursor        (fromDocument, fromNode, node, child
                                        , content, ($/), (&|), ($|)
                                        )

import           Text.Parsec.TX.Utils   (SimpleStringRep (..), deriveJsonS,
                                         derivePersistFieldS,
                                         makeSimpleParserByTable)
import           Yesod.Helpers.Parsec   (derivePathPieceS)

import WeiXin.PublicPlatform.Types


-- | 微信企业支付所产生的订单号
newtype WxMchTransOrderNo = WxMchTransOrderNo { unWxMchTransOrderNo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , NFData
           , ToMessage, ToMarkup)


newtype WxPayAppKey = WxPayAppKey { unWxPayAppKey :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , NFData
           , ToMessage, ToMarkup)


newtype WxPaySignature = WxPaySignature { unWxPaySignature :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , NFData
           , ToMessage, ToMarkup)


-- | 微信支付的设备号
-- 至于 Info 这个词是因为文档也是用这个词的
newtype WxPayDeviceInfo = WxPayDeviceInfo { unWxPayDeviceInfo :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
           , NFData
           , ToMessage, ToMarkup)


-- | 微信支付接口的结果代码
-- 注意: 有两种结果代码: 返回状态状态码, 业务状态码
--       目前看, 内容是一致的
data WxPayResultCode = WxPaySuccess
                     | WxPayFail
                     deriving (Show, Eq, Ord, Enum, Bounded)

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


-- | 微信支付错误代码
newtype WxPayErrorCode = WxPayErrorCode { unWxPayErrorCode :: Text }
  deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary
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
  WxPaySignature $ toUpper $ fromString $
    C8.unpack $ B16.encode $ MD5.hash $ encodeUtf8 str_to_sign
  where
    params_all  = insertMap "nonce_str" nonce_str $ params
    mks k v     = mconcat [ k, "=", v ]
    str_to_sign = intercalate "&" $
                    fmap (uncurry mks) $
                      filter (not . null . snd) $
                        (sortBy (comparing fst) (mapToList params_all)) <> [("key", ak)]


-- | 微信支付调用XML
wxPayOutgoingXmlDoc :: WxPayAppKey
                    -> WxPayParams
                    -- ^ not including: nonce_str, key
                    -> Nonce
                    -> Document
wxPayOutgoingXmlDoc app_key params nonce@(Nonce raw_nonce) =
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
