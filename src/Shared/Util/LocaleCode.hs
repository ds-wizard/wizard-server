module Shared.Util.LocaleCode (
  normalizeLocaleCode,
  isValidLocaleCode,
  primarySubtag,
  matchLocaleCode,
  matchAcceptLanguage,
  parseAcceptLanguage,
) where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace, toLower, toUpper)
import qualified Data.List as L
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Text.Read (readMaybe)

import Shared.Util.String (splitOn)

normalizeLocaleCode :: String -> String
normalizeLocaleCode = L.intercalate "-" . normalizeSubtags . filter (not . null) . splitSubtags . strip

isValidLocaleCode :: String -> Bool
isValidLocaleCode code =
  case splitOn "-" code of
    language : subtags -> length language `elem` [2, 3] && all isAsciiLetter language && all isValidSubtag subtags
    [] -> False
  where
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c
    isValidSubtag subtag = not (null subtag) && length subtag <= 8 && all (\c -> isAsciiLetter c || isDigit c) subtag

primarySubtag :: String -> String
primarySubtag = fmap toLower . takeWhile (\c -> c /= '-' && c /= '_')

matchLocaleCode :: String -> [(String, a)] -> Maybe a
matchLocaleCode requested locales = lookup code normalized <|> lookup (primarySubtag code) normalized
  where
    code = normalizeLocaleCode requested
    normalized = fmap (\(localeCode, value) -> (normalizeLocaleCode localeCode, value)) locales

matchAcceptLanguage :: String -> [(String, a)] -> Maybe a
matchAcceptLanguage header locales = listToMaybe . mapMaybe (`matchLocaleCode` locales) . parseAcceptLanguage $ header

parseAcceptLanguage :: String -> [String]
parseAcceptLanguage = fmap fst . L.sortOn (negate . snd) . filter (\(language, q) -> q > 0 && not (null language)) . fmap parseItem . splitOn ","

normalizeSubtags :: [String] -> [String]
normalizeSubtags [] = []
normalizeSubtags (language : rest) = fmap toLower language : fmap normalizeSubtag rest

normalizeSubtag :: String -> String
normalizeSubtag subtag
  | length subtag == 2 = fmap toUpper subtag
  | length subtag == 3 && all isDigit subtag = subtag
  | length subtag == 4 = titleCase subtag
  | otherwise = fmap toLower subtag
  where
    titleCase (c : cs) = toUpper c : fmap toLower cs
    titleCase [] = []

splitSubtags :: String -> [String]
splitSubtags value =
  case break (\c -> c == '-' || c == '_') value of
    (subtag, _ : rest) -> subtag : splitSubtags rest
    (subtag, []) -> [subtag]

parseItem :: String -> (String, Double)
parseItem item =
  let (language, qPart) = break (== ';') item
   in (strip language, fromMaybe 1.0 (parseQValue qPart))

parseQValue :: String -> Maybe Double
parseQValue param =
  case dropWhile isSpace param of
    ';' : rest ->
      case dropWhile isSpace rest of
        'q' : '=' : value -> readMaybe (takeWhile (\c -> c /= ';' && not (isSpace c)) value)
        _ -> Nothing
    _ -> Nothing

strip :: String -> String
strip = L.dropWhileEnd isSpace . dropWhile isSpace
