module Shared.Util.Gettext where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import qualified Data.Char as CH
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Shared.Util.LocaleCode (normalizeLocaleCode, primarySubtag)
import Shared.Util.String (splitOn)

data PotEntry = PotEntry
  { references :: [String]
  , msgid :: String
  }
  deriving (Show, Eq)

data PoEntry = PoEntry
  { msgctxt :: Maybe String
  , msgid :: String
  , msgidPlural :: Maybe String
  , msgstrs :: [String]
  , flags :: [String]
  }
  deriving (Show, Eq)

data PoHeaderInfo = PoHeaderInfo
  { code :: String
  , name :: String
  , potVersion :: Maybe String
  , kind :: Maybe String
  , version :: Maybe String
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------------------------------------------------
-- POT SERIALIZATION
-- ---------------------------------------------------------------------------------------------------------------------
serializePot :: [(String, String)] -> [PotEntry] -> String
serializePot headers entries = L.intercalate "\n\n" (headerBlock : fmap entryBlock entries) ++ "\n"
  where
    headerBlock =
      L.intercalate "\n" $
        ["msgid \"\"", "msgstr \"\""]
          ++ fmap (\(k, v) -> "\"" ++ escapePoString (k ++ ": " ++ v) ++ "\\n\"") headers
    entryBlock entry =
      L.intercalate "\n" $
        fmap ("#: " ++) entry.references
          ++ ["msgid \"" ++ escapePoString entry.msgid ++ "\"", "msgstr \"\""]

escapePoString :: String -> String
escapePoString = concatMap escape
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape c = [c]

unescapePoString :: String -> String
unescapePoString ('\\' : c : rest) =
  case c of
    'n' -> '\n' : unescapePoString rest
    't' -> '\t' : unescapePoString rest
    'r' -> '\r' : unescapePoString rest
    '"' -> '"' : unescapePoString rest
    '\\' -> '\\' : unescapePoString rest
    _ -> c : unescapePoString rest
unescapePoString (c : rest) = c : unescapePoString rest
unescapePoString [] = []

-- ---------------------------------------------------------------------------------------------------------------------
-- PO HEADER PARSING
-- ---------------------------------------------------------------------------------------------------------------------
parsePoHeaderFields :: String -> Either String [(String, String)]
parsePoHeaderFields content =
  case findHeaderText . groupStatements . fmap classifyLine . lines . stripBom $ content of
    Just headerText -> Right . parseHeaderFields $ headerText
    Nothing -> Left "Missing PO header entry (msgid \"\" with msgstr)"

getPoHeaderField :: String -> [(String, String)] -> Maybe String
getPoHeaderField name fields =
  case lookup name fields of
    Just value
      | not (null value) -> Just value
    _ -> Nothing

-- ---------------------------------------------------------------------------------------------------------------------
-- PO CATALOGUE PARSING
-- ---------------------------------------------------------------------------------------------------------------------
parsePo :: BS.ByteString -> Either String [PoEntry]
parsePo content =
  case TE.decodeUtf8' content of
    Right text -> parseEntries [] . groupTokens . fmap classifyLine . lines . stripBom . T.unpack $ text
    Left _ -> Left "PO file is not valid UTF-8"

poHeaderFields :: [PoEntry] -> [(String, String)]
poHeaderFields entries =
  case L.find isHeaderEntry entries of
    Just PoEntry {msgstrs = headerText : _} -> parseHeaderFields headerText
    _ -> []

poHeaderInfo :: [(String, String)] -> Maybe PoHeaderInfo
poHeaderInfo fields = do
  code <- normalizeLocaleCode <$> getPoHeaderField "Language" fields
  let rawPotVersion = getPoHeaderField "Project-Id-Version" fields
      projectIdVersion = rawPotVersion >>= splitPotVersion
  return
    PoHeaderInfo
      { code = code
      , name = fromMaybe code (getPoHeaderField "X-Display-Name" fields)
      , potVersion = projectIdVersion *> rawPotVersion
      , kind = fmap fst projectIdVersion
      , version = fmap snd projectIdVersion
      }

poPotVersion :: [(String, String)] -> Maybe String
poPotVersion fields = do
  value <- getPoHeaderField "Project-Id-Version" fields
  value <$ splitPotVersion value

-- ---------------------------------------------------------------------------------------------------------------------
-- JED
-- ---------------------------------------------------------------------------------------------------------------------
toJed :: [PoEntry] -> Value
toJed entries =
  object
    [ "domain" .= jedDomain
    , "locale_data" .= object [K.fromString jedDomain .= Object (KM.insert "" (jedHeader entries) (jedMessages entries))]
    ]

-- ---------------------------------------------------------------------------------------------------------------------
-- PLURAL-FORMS
-- ---------------------------------------------------------------------------------------------------------------------
-- Gettext Plural-Forms header value for a language code (e.g. "en", "cs", "pt-BR").
pluralForms :: String -> String
pluralForms code =
  case primarySubtag code of
    "ja" -> onePlural
    "ko" -> onePlural
    "zh" -> onePlural
    "vi" -> onePlural
    "th" -> onePlural
    "id" -> onePlural
    "tr" -> onePlural
    "fr" -> "nplurals=2; plural=(n > 1);"
    "cs" -> "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;"
    "sk" -> "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;"
    "pl" -> "nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);"
    "ru" -> slavicThreePlurals
    "uk" -> slavicThreePlurals
    "sr" -> slavicThreePlurals
    "hr" -> slavicThreePlurals
    "lt" -> "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && (n%100<10 || n%100>=20) ? 1 : 2);"
    "lv" -> "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n != 0 ? 1 : 2);"
    "ro" -> "nplurals=3; plural=(n==1 ? 0 : (n==0 || (n%100 > 0 && n%100 < 20)) ? 1 : 2);"
    "sl" -> "nplurals=4; plural=(n%100==1 ? 0 : n%100==2 ? 1 : n%100==3 || n%100==4 ? 2 : 3);"
    "ar" -> "nplurals=6; plural=(n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 ? 4 : 5);"
    _ -> twoPlurals
  where
    onePlural = "nplurals=1; plural=0;"
    twoPlurals = "nplurals=2; plural=(n != 1);"
    slavicThreePlurals = "nplurals=3; plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);"

-- ---------------------------------------------------------------------------------------------------------------------
-- PRIVATE
-- ---------------------------------------------------------------------------------------------------------------------
data PoLine
  = PoKeywordLine String String
  | PoContinuationLine String
  | PoFlagLine [String]
  | PoIgnoredLine
  deriving (Show, Eq)

data PoToken
  = PoStatement String String
  | PoFlags [String]
  deriving (Show, Eq)

stripBom :: String -> String
stripBom ('\xfeff' : rest) = rest
stripBom content = content

classifyLine :: String -> PoLine
classifyLine rawLine =
  case strip rawLine of
    [] -> PoIgnoredLine
    '#' : ',' : flagText -> PoFlagLine . filter (not . null) . fmap strip . splitOn "," $ flagText
    line@(firstChar : _)
      | firstChar == '#' -> PoIgnoredLine
      | firstChar == '"' -> PoContinuationLine (unquote line)
      | otherwise ->
          let keyword = takeWhile (\c -> not (isSpace c) && c /= '"') line
           in PoKeywordLine keyword (unquote . drop (length keyword) $ line)
  where
    unquote line =
      case dropWhile (/= '"') line of
        '"' : rest -> unescapePoString . takeQuoted $ rest
        _ -> ""
    takeQuoted ('\\' : c : rest) = '\\' : c : takeQuoted rest
    takeQuoted ('"' : _) = []
    takeQuoted (c : rest) = c : takeQuoted rest
    takeQuoted [] = []

groupStatements :: [PoLine] -> [(String, String)]
groupStatements = mapMaybe toStatement . groupTokens
  where
    toStatement (PoStatement keyword value) = Just (keyword, value)
    toStatement (PoFlags _) = Nothing

groupTokens :: [PoLine] -> [PoToken]
groupTokens (PoKeywordLine keyword value : rest) = collectStatement keyword [value] rest
groupTokens (PoFlagLine flags : rest) = PoFlags flags : groupTokens rest
groupTokens (_ : rest) = groupTokens rest
groupTokens [] = []

collectStatement :: String -> [String] -> [PoLine] -> [PoToken]
collectStatement keyword chunks (PoContinuationLine value : rest) = collectStatement keyword (value : chunks) rest
collectStatement keyword chunks (PoIgnoredLine : rest) = collectStatement keyword chunks rest
collectStatement keyword chunks rest =
  let value = concat (reverse chunks)
   in length value `seq` (PoStatement keyword value : groupTokens rest)

parseEntries :: [String] -> [PoToken] -> Either String [PoEntry]
parseEntries _ [] = Right []
parseEntries flags (PoFlags newFlags : rest) = parseEntries (flags ++ newFlags) rest
parseEntries flags (PoStatement "msgctxt" context : PoStatement "msgid" msgid : rest) = parseEntry flags (Just context) msgid rest
parseEntries flags (PoStatement "msgid" msgid : rest) = parseEntry flags Nothing msgid rest
parseEntries _ (PoStatement keyword _ : _) = Left ("Unexpected keyword " ++ keyword)

parseEntry :: [String] -> Maybe String -> String -> [PoToken] -> Either String [PoEntry]
parseEntry flags msgctxt msgid tokens =
  case span isMsgstr afterPlural of
    ([], _) -> Left ("Missing msgstr for msgid \"" ++ msgid ++ "\"")
    (msgstrTokens, rest) -> (entry (fmap tokenValue msgstrTokens) :) <$> parseEntries [] rest
  where
    (msgidPlural, afterPlural) =
      case tokens of
        PoStatement "msgid_plural" plural : rest -> (Just plural, rest)
        _ -> (Nothing, tokens)
    isMsgstr (PoStatement keyword _) = "msgstr" `L.isPrefixOf` keyword
    isMsgstr _ = False
    tokenValue (PoStatement _ value) = value
    tokenValue _ = ""
    entry msgstrs = PoEntry {msgctxt = msgctxt, msgid = msgid, msgidPlural = msgidPlural, msgstrs = msgstrs, flags = flags}

isHeaderEntry :: PoEntry -> Bool
isHeaderEntry entry = null entry.msgid && null (fromMaybe "" entry.msgctxt)

splitPotVersion :: String -> Maybe (String, String)
splitPotVersion value =
  case L.elemIndices ':' value of
    [] -> Nothing
    indices ->
      case splitAt (last indices) value of
        (kind, _ : version)
          | not (null kind) && not (null version) -> Just (kind, version)
        _ -> Nothing

jedDomain :: String
jedDomain = "messages"

jedHeader :: [PoEntry] -> Value
jedHeader entries =
  object $
    ["domain" .= jedDomain]
      ++ maybe [] (\value -> ["plural_forms" .= value]) (lookup "plural-forms" fields)
      ++ maybe [] (\value -> ["lang" .= value]) (lookup "language" fields)
  where
    fields = reverse . fmap (\(k, v) -> (fmap CH.toLower k, v)) . poHeaderFields $ entries

jedMessages :: [PoEntry] -> KM.KeyMap Value
jedMessages =
  fmap toJSON . KM.mapMaybe jedTranslation . KM.fromList . fmap (\entry -> (K.fromString (jedKey entry), entry)) . filter (not . isHeaderEntry)

jedKey :: PoEntry -> String
jedKey entry =
  case entry.msgctxt of
    Just context | not (null context) -> context ++ "\x04" ++ entry.msgid
    _ -> entry.msgid

jedTranslation :: PoEntry -> Maybe [Maybe String]
jedTranslation entry
  | "fuzzy" `elem` entry.flags = Nothing
  | otherwise =
      case entry.msgstrs of
        first : rest -> Just (Just first : fmap (\value -> if null value then entry.msgidPlural else Just value) rest)
        [] -> Just []

strip :: String -> String
strip = L.dropWhileEnd isSpace . dropWhile isSpace

findHeaderText :: [(String, String)] -> Maybe String
findHeaderText (("msgid", "") : ("msgstr", headerText) : _) = Just headerText
findHeaderText (_ : rest) = findHeaderText rest
findHeaderText [] = Nothing

parseHeaderFields :: String -> [(String, String)]
parseHeaderFields = concatMap parseField . lines
  where
    parseField headerLine =
      case break (== ':') headerLine of
        (name, ':' : value) -> [(strip name, strip value)]
        _ -> []
