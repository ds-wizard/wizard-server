module Specs.Util.GettextSpec where

import Control.Exception (evaluate)
import Data.Aeson (Value, decodeStrict)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Timeout (timeout)
import Test.Hspec

import Shared.Util.Gettext

gettextSpec =
  describe "Gettext" $ do
    describe "serializePot" $ do
      it "serializes header and entries" $ do
        -- GIVEN:
        let headers = [("Project-Id-Version", "global:core:1.0.0"), ("Language", "en")]
        let entries =
              [ PotEntry {references = ["chapter/5c135d55-1746-4a51-a739-7d495ac4a2ae/title"], msgid = "Chapter 1"}
              , PotEntry {references = [], msgid = "Multi\nline \"quoted\" \\ text"}
              ]
        -- WHEN:
        let result = serializePot headers entries
        -- THEN:
        let expected =
              L.intercalate
                "\n"
                [ "msgid \"\""
                , "msgstr \"\""
                , "\"Project-Id-Version: global:core:1.0.0\\n\""
                , "\"Language: en\\n\""
                , ""
                , "#: chapter/5c135d55-1746-4a51-a739-7d495ac4a2ae/title"
                , "msgid \"Chapter 1\""
                , "msgstr \"\""
                , ""
                , "msgid \"Multi\\nline \\\"quoted\\\" \\\\ text\""
                , "msgstr \"\""
                , ""
                ]
        result `shouldBe` expected
      it "round-trips the header through the parser" $ do
        -- GIVEN:
        let headers = [("Language", "cs"), ("Plural-Forms", pluralForms "cs")]
        -- WHEN:
        let result = parsePoHeaderFields (serializePot headers [])
        -- THEN:
        result `shouldBe` Right headers
    describe "parsePoHeaderFields" $ do
      it "parses a realistic PO file with comments and multiline strings" $ do
        -- GIVEN:
        let po =
              L.intercalate
                "\n"
                [ "# Czech translation"
                , "#, fuzzy"
                , "msgid \"\""
                , "msgstr \"\""
                , "\"Project-Id-Version: global:core:1.0.0\\n\""
                , "\"Language: cs\\n\""
                , "\"Content-Type: text/plain; charset=UTF-8\\n\""
                , ""
                , "#: chapter/uuid/title"
                , "msgid \"Chapter 1\""
                , "msgstr \"Kapitola 1\""
                , ""
                , "msgid \"Multi\""
                , "\"line\""
                , "msgstr \"\""
                ]
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        fmap (getPoHeaderField "Language") result `shouldBe` Right (Just "cs")
        fmap (getPoHeaderField "Project-Id-Version") result `shouldBe` Right (Just "global:core:1.0.0")
        fmap (getPoHeaderField "Content-Type") result `shouldBe` Right (Just "text/plain; charset=UTF-8")
      it "tolerates plural entries and msgctxt" $ do
        -- GIVEN:
        let po =
              L.intercalate
                "\n"
                [ "msgid \"\""
                , "msgstr \"\""
                , "\"Language: de\\n\""
                , ""
                , "msgctxt \"context\""
                , "msgid \"One item\""
                , "msgid_plural \"More items\""
                , "msgstr[0] \"Ein Element\""
                , "msgstr[1] \"Mehrere Elemente\""
                ]
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        fmap (getPoHeaderField "Language") result `shouldBe` Right (Just "de")
      it "fails when the header entry is missing" $ do
        -- GIVEN:
        let po = "msgid \"Chapter 1\"\nmsgstr \"Kapitola 1\"\n"
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        result `shouldBe` Left "Missing PO header entry (msgid \"\" with msgstr)"
      it "returns Nothing for a missing or empty Language field" $ do
        -- GIVEN:
        let po = "msgid \"\"\nmsgstr \"\"\n\"Content-Type: text/plain\\n\"\n"
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        fmap (getPoHeaderField "Language") result `shouldBe` Right Nothing
    describe "pluralForms" $ do
      it "returns language specific plural forms" $ do
        pluralForms "cs" `shouldBe` "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;"
        pluralForms "en" `shouldBe` "nplurals=2; plural=(n != 1);"
        pluralForms "ja" `shouldBe` "nplurals=1; plural=0;"
        pluralForms "fr" `shouldBe` "nplurals=2; plural=(n > 1);"
      it "uses the primary subtag and falls back to two plural forms" $ do
        pluralForms "CS-cz" `shouldBe` pluralForms "cs"
        pluralForms "pt_BR" `shouldBe` "nplurals=2; plural=(n != 1);"
        pluralForms "xx" `shouldBe` "nplurals=2; plural=(n != 1);"
    describe "parsePo" $ do
      it "parses context, plurals, flags, multi-line strings and escapes" $ do
        -- GIVEN:
        let po =
              L.intercalate
                "\n"
                [ "#, fuzzy"
                , "msgid \"\""
                , "msgstr \"\""
                , "\"Language: de\\n\""
                , ""
                , "#. extracted"
                , "#: src/Page.elm:1"
                , "msgctxt \"button\""
                , "msgid \"Open\""
                , "msgstr \"Öffnen\""
                , ""
                , "#, fuzzy, elm-format"
                , "msgid \"%s file\""
                , "msgid_plural \"%s files\""
                , "msgstr[0] \"%s Datei\""
                , "msgstr[1] \"\""
                , ""
                , "msgid \"\""
                , "\"Two  spaces \""
                , "\"\\\"quoted\\\"\\tand\\\\\""
                , "msgstr \"x\""
                , ""
                , "#~ msgid \"Obsolete\""
                , "#~ msgstr \"Veraltet\""
                ]
        -- WHEN:
        let result = parsePo (encode po)
        -- THEN:
        result
          `shouldBe` Right
            [ PoEntry {msgctxt = Nothing, msgid = "", msgidPlural = Nothing, msgstrs = ["Language: de\n"], flags = ["fuzzy"]}
            , PoEntry {msgctxt = Just "button", msgid = "Open", msgidPlural = Nothing, msgstrs = ["Öffnen"], flags = []}
            , PoEntry {msgctxt = Nothing, msgid = "%s file", msgidPlural = Just "%s files", msgstrs = ["%s Datei", ""], flags = ["fuzzy", "elm-format"]}
            , PoEntry {msgctxt = Nothing, msgid = "Two  spaces \"quoted\"\tand\\", msgidPlural = Nothing, msgstrs = ["x"], flags = []}
            ]
      it "parses a large multi-line msgstr in linear time" $ do
        -- GIVEN:
        let continuation = "\"0123456789\"\n"
        let po = BS.concat ["msgid \"Long\"\nmsgstr \"\"\n", BS.concat (replicate 200000 continuation)]
        -- WHEN:
        result <- timeout 5000000 . evaluate . either (const 0) (sum . fmap (sum . fmap length . (.msgstrs))) $ parsePo po
        -- THEN:
        result `shouldBe` Just 2000000
      it "fails on a msgid without msgstr" $ do
        parsePo "msgid \"Open\"\nmsgid \"Close\"\nmsgstr \"\"\n" `shouldBe` Left "Missing msgstr for msgid \"Open\""
      it "fails on an unexpected keyword" $ do
        parsePo "msgstr \"Open\"\n" `shouldBe` Left "Unexpected keyword msgstr"
      it "fails on invalid UTF-8" $ do
        parsePo "msgid \"\xff\"\nmsgstr \"\"\n" `shouldBe` Left "PO file is not valid UTF-8"
    describe "poHeaderInfo" $ do
      it "reads code, name and the POT version" $ do
        -- GIVEN:
        let fields = [("Language", "pt_br"), ("X-Display-Name", "Português"), ("Project-Id-Version", "fw:client:5.0.0")]
        -- WHEN:
        let result = poHeaderInfo fields
        -- THEN:
        result
          `shouldBe` Just
            PoHeaderInfo {code = "pt-BR", name = "Português", potVersion = Just "fw:client:5.0.0", kind = Just "fw:client", version = Just "5.0.0"}
      it "falls back to the code and accepts a missing or unparsable POT version" $ do
        let expected = Just PoHeaderInfo {code = "de", name = "de", potVersion = Nothing, kind = Nothing, version = Nothing}
        poHeaderInfo [("Language", "de")] `shouldBe` expected
        poHeaderInfo [("Language", "de"), ("Project-Id-Version", "PACKAGE VERSION")] `shouldBe` expected
        poHeaderInfo [("Language", "de"), ("Project-Id-Version", "fw-client:")] `shouldBe` expected
      it "returns Nothing without Language" $ do
        poHeaderInfo [("X-Display-Name", "Deutsch")] `shouldBe` Nothing
        poHeaderInfo [("Language", "")] `shouldBe` Nothing
      it "reads the header fields of parsed entries" $ do
        let entries = parsePo "msgid \"\"\nmsgstr \"\"\n\"Language: de\\n\"\n\"X-Display-Name: Deutsch\\n\"\n"
        fmap (poHeaderInfo . poHeaderFields) entries
          `shouldBe` Right (Just PoHeaderInfo {code = "de", name = "Deutsch", potVersion = Nothing, kind = Nothing, version = Nothing})
    describe "toJed" $ do
      it "produces the po2json jed output" $ do
        -- GIVEN:
        po <- BS.readFile (fixtureDir ++ "de.po")
        expected <- decodeStrict <$> BS.readFile (fixtureDir ++ "de.json")
        -- WHEN:
        let result = toJed <$> parsePo po
        -- THEN:
        fmap Just result `shouldBe` Right (expected :: Maybe Value)

fixtureDir :: String
fixtureDir = "test/shared/Specs/Util/Data/Gettext/"

encode :: String -> BS.ByteString
encode = TE.encodeUtf8 . T.pack
