module WeiXin.PublicPlatform.Types where

import ClassyPrelude
import Data.SafeCopy
import Data.Aeson
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as C8


newtype Token = Token { unToken :: Text }
                    deriving (Show, Eq)

newtype AesKey = AesKey { unAesKey :: ByteString }
                    deriving (Eq)
instance Show AesKey where
    show (AesKey bs) = "AesKey:" <> (C8.unpack $ B64.encode bs)

newtype TimeStampS = TimeStampS { unTimeStampS :: Text }
                    deriving (Show, Eq)

newtype Nonce = Nonce { unNounce :: Text }
                    deriving (Show, Eq)

newtype AccessToken = AccessToken { unAccessToken :: Text }
                    deriving (Show, Eq, Typeable)
$(deriveSafeCopy 0 'base ''AccessToken)


newtype WxppAppID = WxppAppID { unWxppAppID :: Text }
                    deriving (Show, Eq)

newtype WxppAppSecret = WxppAppSecret { unWxppAppSecret :: Text }
                    deriving (Show, Eq)

data WxppAppConfig = WxppAppConfig {
                    wxppConfigAppID         :: WxppAppID
                    , wxppConfigAppSecret   :: WxppAppSecret
                    }
                    deriving (Show, Eq)

wxppLogSource :: IsString a => a
wxppLogSource = "WXPP"
