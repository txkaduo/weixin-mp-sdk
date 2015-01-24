module WeiXin.PublicPlatform.Types where

import ClassyPrelude


newtype Token = Token { unToken :: Text }
                    deriving (Show, Eq)

newtype AesKey = AesKey { unAesKey :: ByteString }
                    deriving (Show, Eq)

newtype TimeStampS = TimeStampS { unTimeStampS :: Text }
                    deriving (Show, Eq)

newtype Nonce = Nonce { unNounce :: Text }
                    deriving (Show, Eq)

newtype AccessToken = AccessToken { unAccessToken :: Text }
                    deriving (Show, Eq)


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
