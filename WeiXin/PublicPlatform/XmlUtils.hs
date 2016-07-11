module WeiXin.PublicPlatform.XmlUtils where

import           ClassyPrelude
import qualified Data.Text                  as T
import           Text.XML
import           Text.XML.Cursor


getElementContent :: Cursor -> Name -> Either String Text
getElementContent cursor t =
    maybe (Left $ T.unpack $ "no such element: " <> nameLocalName t) Right $
        getElementContentMaybe cursor t

getElementContentMaybe :: Cursor -> Name -> Maybe Text
getElementContentMaybe cursor t =
    listToMaybe $ cursor $/ element t &// content
