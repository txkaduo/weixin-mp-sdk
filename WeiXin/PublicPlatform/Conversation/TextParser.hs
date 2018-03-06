module WeiXin.PublicPlatform.Conversation.TextParser where

import ClassyPrelude.Yesod hiding ((<|>), try, FilePath, (</>), Proxy)
import qualified Data.Text                  as T
import Data.Char
import Text.Read                            (reads)

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos

import Yesod.Helpers.Parsec
import Yesod.Helpers.Utils                  (toHalfWidthDigit)


parseTextWholeLine :: (IsString a, Stream s m Char) => ParsecT s u m a
parseTextWholeLine = fmap fromString $ manyTill anyChar $ try eof <|> try (eol *> return ())

parseTextWholeLineStripped :: Stream s m Char => ParsecT s u m Text
parseTextWholeLineStripped = liftM T.strip parseTextWholeLine

parseTextWholeLineStrippedNonempty :: Stream s m Char => ParsecT s u m Text
parseTextWholeLineStrippedNonempty = do
    s <- parseTextWholeLineStripped

    when ( T.null s ) $ do
      unexpected "没有输入"

    return s

parseTextBool :: Stream s m Char => ParsecT s u m Bool
parseTextBool = do
    spaces
    name <- T.toLower . T.strip . fromString <$> many1 anyChar
    if name `elem` [ "是", "yes" ]
        then return True
        else
            if name `elem` [ "不", "否", "no" ]
                then return False
                else mzero


generalParseConfirm :: Stream s m Char => ParsecT s u m Bool
generalParseConfirm = do
    spaces
    name <- T.toLower . T.strip . fromString <$> many1 anyChar
    if name `elem` [ "是", "同意", "好", "好的", "yes" ]
        then return True
        else
            if name `elem` [ "否", "拒绝", "不", "不好", "不要", "no" ]
                then return False
                else mzero

generalParseSave :: Stream s m Char => ParsecT s u m Bool
generalParseSave = do
    spaces
    name <- T.toLower . T.strip . fromString <$> many1 anyChar
    if name `elem` [ "保存", "确定", "save" ]
        then return True
        else
            if name `elem` [ "放弃", "discard" ]
                then return False
                else mzero


parsePossibleFullWithDecimalNumber :: (Stream s m Char, Read a) => ParsecT s u m a
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

showParseErrorZh :: IsString a => ParseError -> a
showParseErrorZh = fromString . showErrorMessages "或" "未知解释错误" "可接受" "非法输入" "结束输入" . errorMessages

keyword :: Stream s m Char => String -> ParsecT s u m String
keyword s = do
  when (not $ null $ filter (not . isPrint) s ) $ do
    error $ "关键字包含不可见字符: " <> show s

  tokens showKeyword updatePosString s

  where
    showKeyword k = "【" <> k <> "】"
