{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import ClassyPrelude
import Crypto.Cipher                        (makeKey)
import System.Exit
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base16     as B16
import Data.Default                         (def)
import Text.XML                             (renderText)
import qualified Data.Text.Lazy             as LT
import qualified Data.Text                  as T
import qualified Data.Aeson                 as A
import Data.Aeson                           (ToJSON(..))

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
    putStrLn $ f $ WxppOutMsgEntity (WxppOpenID "openID收") "一个人" now $
                        WxppOutMsgText "中文\n<xml>"


testCharParser :: (Eq a, Show a) =>
    CharParser a -> Text -> a -> IO ()
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
  let sign = wxPaySign
              (WxPayAppKey "192006250b4c09247ec02edce69f6a2d")
              [ ("appid", "wxd930ea5d5a258f4f")
              , ("mch_id", "10000100")
              , ("device_info", "1000")
              , ("body", "test")
              ]
              (Nonce "ibuaiVcKdpRxkhJA")

  if sign /= WxPaySignature "9A0A8659F005D6984697E2CA0A9CF3B7"
        then do
            putStrLn $ "wrong pay signature: " <> tshow sign
            exitFailure
        else
            putStrLn $ "pay signature OK: " <> tshow sign


main :: IO ()
main = do
    testWxPaySign
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
