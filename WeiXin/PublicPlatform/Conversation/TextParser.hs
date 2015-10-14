module WeiXin.PublicPlatform.Conversation.TextParser where

import ClassyPrelude.Yesod hiding ((<|>), try, FilePath, (</>), Proxy)
import qualified Data.Text                  as T
import Data.Char
import Text.Read                            (reads)

import Text.Parsec

import Yesod.Helpers.Parsec
import Yesod.Helpers.Utils                  (toHalfWidthDigit)


parseTextWholeLine :: IsString s => CharParser s
parseTextWholeLine = fmap fromString $ manyTill anyChar $ try eof <|> try (eol *> return ())

parseTextWholeLineStripped :: CharParser Text
parseTextWholeLineStripped = liftM T.strip parseTextWholeLine

parseTextWholeLineStrippedNonempty :: CharParser Text
parseTextWholeLineStrippedNonempty = do
    s <- parseTextWholeLineStripped
    guard $ not $ T.null s
    return s

parseTextBool :: CharParser Bool
parseTextBool = do
    spaces
    name <- T.toLower . T.strip . fromString <$> many1 anyChar
    if name `elem` [ "是", "yes" ]
        then return True
        else
            if name `elem` [ "不", "否", "no" ]
                then return False
                else mzero


generalParseConfirm :: CharParser Bool
generalParseConfirm = do
    spaces
    name <- T.toLower . T.strip . fromString <$> many1 anyChar
    if name `elem` [ "是", "同意", "好", "好的", "yes" ]
        then return True
        else
            if name `elem` [ "否", "拒绝", "不", "不好", "不要", "no" ]
                then return False
                else mzero


parsePossibleFullWithDecimalNumber :: (Integral a, Read a) => CharParser a
parsePossibleFullWithDecimalNumber = do
    t <- many1 $ satisfy $ ((== DecimalNumber) . generalCategory)
    let t' = map toHalfWidthDigit t
    maybe mzero return $ readAllMaybe t'

readAllMaybe :: Read a => String -> Maybe a
readAllMaybe = fmap fst . listToMaybe . filter (null . snd) . reads

runTextParser :: forall s a. IsString s
                => Parsec String () a
                -> Text
                -> Either s a
runTextParser p t = either (Left . fromString . show) Right $ parse p "" (T.unpack $ T.strip t)

