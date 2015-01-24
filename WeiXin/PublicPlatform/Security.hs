module WeiXin.PublicPlatform.Security where

import ClassyPrelude
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.Text                  as T
import Data.ByteString                      (ByteString)


newtype Token = Token { unToken :: Text }

newtype AesKey = AesKey { unAesKey :: ByteString }

newtype TimeStampS = TimeStampS { unTimeStampS :: Text }

newtype Nonce = Nonce { unNounce :: Text }

wxppSignature :: Token -> TimeStampS -> Nonce -> Text -> ByteString
wxppSignature (Token token) (TimeStampS tt) (Nonce nn) msg =
    SHA1.hash $ encodeUtf8 $ mconcat $ sort [tt, nn, token, msg]
