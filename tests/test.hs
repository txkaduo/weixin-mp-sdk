{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import ClassyPrelude
import Crypto.Cipher                        (makeKey)
import System.Exit
import           Control.Monad.Logger
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base16     as B16
import Data.Default                         (def)
import Text.XML                             (renderText)
import           Text.XML               (Element (..), Name (..),
                                         Node (..), parseLBS)
import           Text.XML.Cursor        (child, content, fromDocument, fromNode
                                        , node, ($/), ($|), (&|)
                                        )
import qualified Data.Text.Lazy             as LT
import qualified Data.Text                  as T
import qualified Data.Aeson                 as A
import Data.Aeson                           (ToJSON(..))
import Data.Time

import Text.Parsec
import Yesod.Helpers.Parsec

import WeiXin.PublicPlatform


testLikeJava :: IO ()
testLikeJava = do
    aes_bs <- case B64.decode $ encodingAesKey <> "=" of
                    Left err -> do
                        putStrLn $ "failed to decode encodingAesKey: "
                                    <> fromString err
                        exitFailure
                    Right bs -> return bs
    aes_key <- case makeKey aes_bs of
                    Left err -> do
                        putStrLn $ "failed to makeKey: "
                                    <> fromString (show err)
                        exitFailure
                    Right x -> return $ AesKey x

    case wxppEncryptInternal2 appId aes_key randomStr (encodeUtf8 replyMsg) of
        Left err -> do
                    putStrLn $ "failed to wxppEncryptText: "
                                <> fromString err
                    exitFailure
        Right encrypted -> do
            let real_afterAesEncrypt = fromString $ C8.unpack $ B64.encode encrypted
            when (real_afterAesEncrypt /= afterAesEncrypt) $ do
                putStrLn $ "wxppEncryptText returns unexpected result"
                putStrLn real_afterAesEncrypt
                putStrLn afterAesEncrypt
                exitFailure

            let dec_res = wxppDecrypt appId aes_key encrypted
            case dec_res of
                Left err -> do
                    putStrLn $ "failed to wxppDecrypt: "
                                <> fromString err
                    exitFailure
                Right msg_bs -> do
                    when (msg_bs /= encodeUtf8 replyMsg) $ do
                        putStrLn $ "wxppDecrypt returns unexpected result"
                        putStrLn $ fromString $ C8.unpack $ B64.encode msg_bs
                        exitFailure

    case B64.decode afterAesEncrypt2 of
        Left err -> do
                    putStrLn $ "failed to base64-decode afterAesEncrypt2: "
                                <> fromString err
                    exitFailure
        Right bs -> do
            let dec_res = wxppDecrypt appId aes_key bs
            case dec_res of
                Left err -> do
                    putStrLn $ "failed to wxppDecrypt: " <> fromString err
                    exitFailure
                Right msg_bs -> do
                    putStrLn $ decodeUtf8 msg_bs
                    case wxppInMsgEntityFromLbs $ LB.fromStrict msg_bs of
                        Left err -> do
                            putStrLn $ "failed to wxppMessageNoticeFromLbsA: " <> fromString err
                            exitFailure
                        Right mn -> do
                            -- putStrLn $ decodeUtf8 msg_bs
                            putStrLn $ fromString $ show mn

    where
        -- nonce = "xxxxxx"
        appId = WxppAppID "wxb11529c136998cb6"
        encodingAesKey = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFG"
        -- token = "pamtest"
        randomStr = encodeUtf8 "aaaabbbbccccdddd"
        replyMsg = "我是中文abcd123"
        afterAesEncrypt = "jn1L23DB+6ELqJ+6bruv21Y6MD7KeIfP82D6gU39rmkgczbWwt5+3bnyg5K55bgVtVzd832WzZGMhkP72vVOfg=="
        afterAesEncrypt2 = "jn1L23DB+6ELqJ+6bruv23M2GmYfkv0xBh2h+XTBOKVKcgDFHle6gqcZ1cZrk3e1qjPQ1F4RsLWzQRG9udbKWesxlkupqcEcW7ZQweImX9+wLMa0GaUzpkycA8+IamDBxn5loLgZpnS7fVAbExOkK5DYHBmv5tptA9tklE/fTIILHR8HLXa5nQvFb3tYPKAlHF3rtTeayNf0QuM+UW/wM9enGIDIJHF7CLHiDNAYxr+r+OrJCmPQyTy8cVWlu9iSvOHPT/77bZqJucQHQ04sq7KZI27OcqpQNSto2OdHCoTccjggX5Z9Mma0nMJBU+jLKJ38YB1fBIz+vBzsYjrTmFQ44YfeEuZ+xRTQwr92vhA9OxchWVINGC50qE/6lmkwWTwGX9wtQpsJKhP+oS7rvTY8+VdzETdfakjkwQ5/Xka042OlUb1/slTwo4RscuQ+RdxSGvDahxAJ6+EAjLt9d8igHngxIbf6YyqqROxuxqIeIch3CssH/LqRs+iAcILvApYZckqmA7FNERspKA5f8GoJ9sv8xmGvZ9Yrf57cExWtnX8aCMMaBropU/1k+hKP5LVdzbWCG0hGwx/dQudYR/eXp3P0XxjlFiy+9DMlaFExWUZQDajPkdPrEeOwofJb"

testMsgToXml :: IO ()
testMsgToXml = do
    let f = LT.toStrict . renderText def . wxppOutMsgEntityToDocument
    now <- getCurrentTime
    putStrLn $ f $ WxppOutMsgEntity (WxppOpenID "openID收") (WeixinUserName "一个人") now $
                        WxppOutMsgText "中文\n<xml>"


testCharParser :: (Eq a, Show a)
               => ParsecT String () Identity a
               -> Text -> a -> IO ()
testCharParser p input expected = do
    case parse p "" (T.unpack input) of
        Left err -> do
                    putStrLn $ "cannot parse input: " <> input
                    putStrLn $ "error was: " <> (T.pack $ show err)
                    exitFailure
        Right x -> do
                if x /= expected
                    then do
                        putStrLn $ "parse result error: " <> (fromString $ show x)
                        putStrLn $ "error was: not equal to the expected: "
                                    <> (fromString $ show expected)
                        exitFailure
                    else return ()

testParseScene :: IO ()
testParseScene = do
    let test = testCharParser simpleParser
    test "qrscene_online2015 bind 123" (WxppSceneStr $ WxppStrSceneID "online2015 bind 123")

showJson :: ToJSON a => a -> IO ()
showJson a = do
    LB.putStr $ A.encode a
    putStrLn ""


testJsApiTicket :: IO ()
testJsApiTicket = do
    let sign = wxppJsApiSignature
                    (WxppJsTicket "sM4AOVdWfPE4DxkXGEs8VMCPGGVi4C3VM0P37wVUCFvkVAy_90u5h9nbSlYy3-Sl-HhTdfl2fzFy1AOcHKP7qg")
                    (UrlText "http://mp.weixin.qq.com?params=value")
                    1414587457
                    (Nonce "Wm3WZYTPz0wzccnW")
        sign_str = C8.unpack (B16.encode sign)
    if sign_str /= "0f9de62fce790f9a083d5c99e95740ceb90c27ed"
        then do
            putStrLn $ "wrong js signature: " <> fromString sign_str
            exitFailure
        else
            putStrLn $ "js signature OK: " <> fromString sign_str

testWxPaySign :: IO ()
testWxPaySign = do
  let app_key = WxPayAppKey "192006250b4c09247ec02edce69f6a2d"
      raw_nonce = "ibuaiVcKdpRxkhJA"
      nonce = Nonce raw_nonce
      params = mapFromList
                [ ("appid", "wxd930ea5d5a258f4f")
                , ("mch_id", "10000100")
                , ("device_info", "1000")
                , ("body", "test")
                ]

  let sign = wxPaySignInternal app_key $ mapToList $ insertMap "nonce_str" raw_nonce params

  if sign /= WxPaySignature "9A0A8659F005D6984697E2CA0A9CF3B7"
        then do
            putStrLn $ "wrong pay signature: " <> tshow sign
            exitFailure
        else
            putStrLn $ "pay signature OK: " <> tshow sign

  let doc = wxPayOutgoingXmlDoc app_key params nonce
  let doc_t = wxPayRenderOutgoingXmlDoc app_key params nonce
  putStrLn $ "WX Pay call document:"
  putStrLn $ LT.toStrict doc_t

  case wxPayParseIncomingXmlDoc (Just app_key) doc of
    Left err -> do
      putStrLn $ "Failed to parse pay doc: " <> err
      exitFailure

    Right (Left err) -> do
      putStrLn $ "Failed to parse pay doc: " <> tshow err
      exitFailure

    Right (Right params') -> do
      when ( params /= params' ) $ do
        putStrLn $ "Failed to parse pay doc: params is not expected"
        exitFailure


testWxMmTransParseTime :: IO ()
testWxMmTransParseTime = do
  let test_it s lt = do
        case wxPayMchTransParseTimeStr s of
          Nothing -> do
            putStrLn $ "Failed to parse time string: " <> fromString s
            exitFailure

          Just lt0 -> do
            when ( lt0 /= lt ) $ do
              putStrLn $ "Time string parse result is wrong: " <> tshow lt0
              exitFailure

  let lt = LocalTime (fromGregorian 2015 5 19) (TimeOfDay 15 26 59)
  test_it "2015-05-19 15：26：59" lt
  test_it "2015-05-19 15:26:59" lt


testWxPayParseBankCode :: IO ()
testWxPayParseBankCode = do
  let test_it s code = do
        case parseBankCode s of
          Nothing -> do
            putStrLn $ "Failed to parse bank code string: " <> fromString s
            exitFailure

          Just code0 -> do
            when ( code0 /= code ) $ do
              putStrLn $ "bank code string parse result is wrong: " <> tshow code0
              exitFailure

  test_it "VISA_CREDIT" VISA_CREDIT

testWxUserPayStateDoc1 :: IO ()
testWxUserPayStateDoc1 = do
  let doc = "<xml>\
  \<appid><![CDATA[wx2421b1c4370ec43b]]></appid>\
  \<attach><![CDATA[支付测试]]></attach>\
  \<bank_type><![CDATA[CMB_DEBIT]]></bank_type>\
  \<fee_type><![CDATA[CNY]]></fee_type>\
  \<is_subscribe><![CDATA[Y]]></is_subscribe>\
  \<mch_id><![CDATA[10000100]]></mch_id>\
  \<nonce_str><![CDATA[5d2b6c2a8db53831f7eda20af46e531c]]></nonce_str>\
  \<openid><![CDATA[oUpF8uMEb4qRXf22hE3X68TekukE]]></openid>\
  \<out_trade_no><![CDATA[1409811653]]></out_trade_no>\
  \<result_code><![CDATA[SUCCESS]]></result_code>\
  \<return_code><![CDATA[SUCCESS]]></return_code>\
  \<sign><![CDATA[B552ED6B279343CB493C5DD0D78AB241]]></sign>\
  \<sub_mch_id><![CDATA[10000100]]></sub_mch_id>\
  \<time_end><![CDATA[20140903131540]]></time_end>\
  \<total_fee>1</total_fee>\
  \<trade_type><![CDATA[JSAPI]]></trade_type>\
  \<transaction_id><![CDATA[1004400740201409030005092168]]></transaction_id>\
  \</xml>"

  pay_stat <- testWxUserPayStateDocHelper doc
  testEq (Just "支付测试") (wxUserPaySuccAttach pay_stat)
  testEq (Just True) (wxUserPaySuccIsSubs pay_stat)

testWxUserPayStateDoc2 :: IO ()
testWxUserPayStateDoc2 = do
  let doc = "<xml>\
   \<return_code><![CDATA[SUCCESS]]></return_code>\
   \<return_msg><![CDATA[OK]]></return_msg>\
   \<appid><![CDATA[wx2421b1c4370ec43b]]></appid>\
   \<mch_id><![CDATA[10000100]]></mch_id>\
   \<device_info><![CDATA[1000]]></device_info>\
   \<nonce_str><![CDATA[TN55wO9Pba5yENl8]]></nonce_str>\
   \<sign><![CDATA[BDF0099C15FF7BC6B1585FBB110AB635]]></sign>\
   \<result_code><![CDATA[SUCCESS]]></result_code>\
   \<openid><![CDATA[oUpF8uN95-Ptaags6E_roPHg7AG0]]></openid>\
   \<is_subscribe><![CDATA[Y]]></is_subscribe>\
   \<trade_type><![CDATA[MICROPAY]]></trade_type>\
   \<bank_type><![CDATA[CCB_DEBIT]]></bank_type>\
   \<total_fee>1</total_fee>\
   \<fee_type><![CDATA[CNY]]></fee_type>\
   \<transaction_id><![CDATA[1008450740201411110005820873]]></transaction_id>\
   \<out_trade_no><![CDATA[1415757673]]></out_trade_no>\
   \<attach><![CDATA[订单额外描述]]></attach>\
   \<time_end><![CDATA[20141111170043]]></time_end>\
   \<trade_state><![CDATA[SUCCESS]]></trade_state>\
   \</xml>"

  pay_stat <- testWxUserPayStateDocHelper doc
  testEq (Just "订单额外描述") (wxUserPaySuccAttach pay_stat)
  testEq (Just True) (wxUserPaySuccIsSubs pay_stat)


testWxUserPayStateDocHelper :: Text -> IO (WxUserPaySuccInfo)
testWxUserPayStateDocHelper = testWxUserParseXmlDocHelper wxUserPayParseStateParams

testWxUserParseXmlDocHelper :: (WxPayParams -> LoggingT IO (Either WxPayDiagError a))
                            -> Text
                            -> IO (a)
testWxUserParseXmlDocHelper f doc_txt = do
  case parseLBS def (fromStrict $ encodeUtf8 doc_txt) of
      Left ex         -> do
        putStrLn $ "Failed to parse XML: " <> tshow ex
        exitFailure

      Right doc  -> do
        let cursor = fromDocument doc
        let all_params = mapFromList $
                          catMaybes $ map param_from_node $
                            cursor $| child &| node

        err_or <- runStderrLoggingT $ f all_params
        case err_or of
          Left err -> do
            putStrLn $ "Xml doc error: " <> tshow err
            exitFailure

          Right x -> return x

  where
    param_from_node n@(NodeElement ele) = do
      v <- listToMaybe $ fromNode n $/ content
      let name = nameLocalName (elementName ele)
      return (name, v)

    param_from_node _ = Nothing


testWxUserPayRefundReqDoc :: IO ()
testWxUserPayRefundReqDoc = do
  -- 这个例子本是从文档原文拷贝下来，但少了 total_fee, cash_fee 字段，与其文字说明不符
  let doc = "<xml>\
   \<return_code><![CDATA[SUCCESS]]></return_code>\
   \<return_msg><![CDATA[OK]]></return_msg>\
   \<appid><![CDATA[wx2421b1c4370ec43b]]></appid>\
   \<mch_id><![CDATA[10000100]]></mch_id>\
   \<nonce_str><![CDATA[NfsMFbUFpdbEhPXP]]></nonce_str>\
   \<sign><![CDATA[B7274EB9F8925EB93100DD2085FA56C0]]></sign>\
   \<result_code><![CDATA[SUCCESS]]></result_code>\
   \<transaction_id><![CDATA[1008450740201411110005820873]]></transaction_id>\
   \<out_trade_no><![CDATA[1415757673]]></out_trade_no>\
   \<out_refund_no><![CDATA[1415701182]]></out_refund_no>\
   \<refund_id><![CDATA[2008450740201411110000174436]]></refund_id>\
   \<refund_channel><![CDATA[]]></refund_channel>\
   \<refund_fee>1</refund_fee>\
   \<total_fee>2</total_fee>\
   \<cash_fee>2</cash_fee>\
   \</xml>"

  refund_req <- testWxUserParseXmlDocHelper wxUserPayParseRefundReqParams doc
  testEq (WxPayMoneyAmount 1) (wxUserPayRefundReqReRefundFee refund_req)
  testEq (WxPayMoneyAmount 2) (wxUserPayRefundReqReTotalFee refund_req)
  testEq (WxPayMoneyAmount 2) (wxUserPayRefundReqReCashFee refund_req)
  testEq (WxUserPayRefundId "2008450740201411110000174436") (wxUserPayRefundReqReRefundId refund_req)
  testEq (WxUserPayOutRefundNo "1415701182") (wxUserPayRefundReqReOutRefundNo refund_req)
  testEq (WxUserPayOutTradeNo "1415757673") (wxUserPayRefundReqReOutTradeNo refund_req)
  testEq Nothing (wxUserPayRefundReqReChannel refund_req)


testWxUserPayRefundQueryDoc :: IO ()
testWxUserPayRefundQueryDoc = do
  -- 这个例子本是从文档原文拷贝下来，但少了 total_fee, cash_fee, refund_recv_account_$n 字段，与其文字说明不符
  let doc = "<xml>\
   \<appid><![CDATA[wx2421b1c4370ec43b]]></appid>\
   \<mch_id><![CDATA[10000100]]></mch_id>\
   \<nonce_str><![CDATA[TeqClE3i0mvn3DrK]]></nonce_str>\
   \<out_refund_no_0><![CDATA[1415701182]]></out_refund_no_0>\
   \<out_trade_no><![CDATA[1415757673]]></out_trade_no>\
   \<refund_count>1</refund_count>\
   \<refund_fee_0>1</refund_fee_0>\
   \<refund_id_0><![CDATA[2008450740201411110000174436]]></refund_id_0>\
   \<refund_status_0><![CDATA[PROCESSING]]></refund_status_0>\
   \<result_code><![CDATA[SUCCESS]]></result_code>\
   \<return_code><![CDATA[SUCCESS]]></return_code>\
   \<return_msg><![CDATA[OK]]></return_msg>\
   \<sign><![CDATA[1F2841558E233C33ABA71A961D27561C]]></sign>\
   \<transaction_id><![CDATA[1008450740201411110005820873]]></transaction_id>\
   \<total_fee>2</total_fee>\
   \<cash_fee>2</cash_fee>\
   \<refund_recv_account_0>支付用户零钱</refund_recv_account_0>\
   \</xml>"

  refund_query <- testWxUserParseXmlDocHelper wxUserPayParseRefundQueryResult doc
  testEq (WxUserPayTransId "1008450740201411110005820873") (wxUserPayRefundQueryReTransId refund_query)
  testEq (WxUserPayOutTradeNo "1415757673") (wxUserPayRefundQueryReOutTradeNo refund_query)

  case wxUserPayRefundQueryReItems refund_query of
    [item] -> do
      testEq (WxUserPayRefundId "2008450740201411110000174436") (wxUserPayRefundQueryItemRefundId item)
      testEq (WxUserPayOutRefundNo "1415701182") (wxUserPayRefundQueryItemOutRefundNo item)
      testEq WxPayRefundProcessing (wxUserPayRefundQueryItemStatus item)
      testEq (WxPayMoneyAmount 1) (wxUserPayRefundQueryItemRefundFee item)

    xs -> do
      putStrLn $ "Wrong wxUserPayRefundQueryReItems numbers: length=" <> tshow (length xs)
      exitFailure


main :: IO ()
main = do
    testWxPaySign
    testWxMmTransParseTime
    testWxPayParseBankCode
    testWxUserPayStateDoc1
    testWxUserPayStateDoc2
    testWxUserPayRefundReqDoc
    testWxUserPayRefundQueryDoc
    testJsApiTicket
    testMsgToXml
    -- testLikeJava
    testParseScene
    showJson $ WxppBriefNews $ return $  WxppBriefArticle
                                                "标题"
                                                (WxppBriefMediaID "xxx-media-id")
                                                Nothing
                                                Nothing
                                                True
                                                "<h1>xxx</h1>"
                                                Nothing
    showJson $ PropagateFilter Nothing

    putStrLn "=========== ALL DONE ==============="


testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq expected real_val = do
  unless (expected == real_val) $ do
    putStrLn $ tshow real_val <> " does not equal to expected: " <> tshow expected
    exitFailure

