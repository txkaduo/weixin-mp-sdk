module Main where

import ClassyPrelude
import Crypto.Cipher                        (makeKey)
import System.Exit
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8

import WeiXin.PublicPlatform.Security


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

            dec_res <- wxppDecrypt appId aes_key encrypted
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
    where
        -- nonce = "xxxxxx"
        appId = WxppAppID "wxb11529c136998cb6"
        encodingAesKey = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFG"
        token = "pamtest"
        randomStr = encodeUtf8 "aaaabbbbccccdddd"
        replyMsg = "我是中文abcd123"
        afterAesEncrypt = "jn1L23DB+6ELqJ+6bruv21Y6MD7KeIfP82D6gU39rmkgczbWwt5+3bnyg5K55bgVtVzd832WzZGMhkP72vVOfg=="
        afterAesEncrypt2 = "jn1L23DB+6ELqJ+6bruv23M2GmYfkv0xBh2h+XTBOKVKcgDFHle6gqcZ1cZrk3e1qjPQ1F4RsLWzQRG9udbKWesxlkupqcEcW7ZQweImX9+wLMa0GaUzpkycA8+IamDBxn5loLgZpnS7fVAbExOkK5DYHBmv5tptA9tklE/fTIILHR8HLXa5nQvFb3tYPKAlHF3rtTeayNf0QuM+UW/wM9enGIDIJHF7CLHiDNAYxr+r+OrJCmPQyTy8cVWlu9iSvOHPT/77bZqJucQHQ04sq7KZI27OcqpQNSto2OdHCoTccjggX5Z9Mma0nMJBU+jLKJ38YB1fBIz+vBzsYjrTmFQ44YfeEuZ+xRTQwr92vhA9OxchWVINGC50qE/6lmkwWTwGX9wtQpsJKhP+oS7rvTY8+VdzETdfakjkwQ5/Xka042OlUb1/slTwo4RscuQ+RdxSGvDahxAJ6+EAjLt9d8igHngxIbf6YyqqROxuxqIeIch3CssH/LqRs+iAcILvApYZckqmA7FNERspKA5f8GoJ9sv8xmGvZ9Yrf57cExWtnX8aCMMaBropU/1k+hKP5LVdzbWCG0hGwx/dQudYR/eXp3P0XxjlFiy+9DMlaFExWUZQDajPkdPrEeOwofJb"

main :: IO ()
main = do
    testLikeJava
