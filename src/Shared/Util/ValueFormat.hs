module Shared.Util.ValueFormat where

import Data.Either (isRight)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time
import Network.URI (parseAbsoluteURI, uriScheme)
import Text.Regex (mkRegex, subRegex)
import Text.Regex.TDFA (Regex, defaultCompOpt, defaultExecOpt, matchTest)
import qualified Text.Regex.TDFA.String as TDFA

import Shared.Util.String (replace)

readDate :: String -> Maybe Day
readDate = parseFirst ["%Y-%m-%d"]

readDateTime :: String -> Maybe LocalTime
readDateTime = parseFirst ["%Y-%m-%dT%H:%M", "%Y-%m-%dT%H:%M:%S%Q", "%Y-%m-%dT%H:%M:%S%QZ"]

readTime :: String -> Maybe TimeOfDay
readTime = parseFirst ["%H:%M", "%H:%M:%S"]

parseFirst :: ParseTime t => [String] -> String -> Maybe t
parseFirst formats value = listToMaybe (mapMaybe (\format -> parseTimeM False defaultTimeLocale format value) formats)

isEmail :: String -> Bool
isEmail = matchesPosixRegex "^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$"

isUrl :: String -> Bool
isUrl = maybe False ((`elem` ["http:", "https:"]) . uriScheme) . parseAbsoluteURI

matchesPosixRegex :: String -> String -> Bool
matchesPosixRegex regex value = either (const False) (`matchTest` value) (compilePosixRegex regex)

compilePosixRegex :: String -> Either String Regex
compilePosixRegex = TDFA.compile defaultCompOpt defaultExecOpt

matchesRegex :: String -> String -> Bool
matchesRegex regex = matchesPosixRegex (toPosixRegex regex)

isRegex :: String -> Bool
isRegex = isRight . compilePosixRegex . toPosixRegex

toPosixRegex :: String -> String
toPosixRegex = toPosixClasses . toPosixSyntax

toPosixClasses :: String -> String
toPosixClasses = replace "\\d" "[0-9]" . replace "\\w" "[A-Za-z0-9_]" . replace "\\s" "[[:space:]]"

toPosixSyntax :: String -> String
toPosixSyntax regex =
  let withPlainGroups = subRegex (mkRegex "\\(\\?(:|=|!|<=|<!|<[A-Za-z][A-Za-z0-9]*>)") regex "("
   in subRegex (mkRegex "([*+?}])\\?") withPlainGroups "\\1"
