module Specs.Util.JSON.CanonicalSpec where

import Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Util.JSON.Canonical

canonicalSpec =
  describe "Canonical JSON (RFC 8785)" $ do
    describe "numbers" $ do
      let cases =
            [ ("1.0", "1")
            , ("-0", "0")
            , ("-0.0", "0")
            , ("0", "0")
            , ("1e21", "1e+21")
            , ("1e+21", "1e+21")
            , ("1e-7", "1e-7")
            , ("0.000001", "0.000001")
            , ("123.456", "123.456")
            , ("9007199254740992", "9007199254740992")
            , ("333333333.3333332", "333333333.3333332")
            , ("5e-324", "5e-324")
            , ("1.7976931348623157e+308", "1.7976931348623157e+308")
            , ("123e-20", "1.23e-18")
            , ("295147905179352830000", "295147905179352830000")
            , ("1e23", "1e+23")
            , ("9.999999999999999e22", "1e+23")
            , ("2e23", "2e+23")
            , ("8.41e21", "8.41e+21")
            , ("4.35e21", "4.35e+21")
            , ("1e300", "1e+300")
            , ("2.2250738585072014e-308", "2.2250738585072014e-308")
            , ("0.1", "0.1")
            , ("0.3", "0.3")
            , ("1e-6", "0.000001")
            , ("12345678901234567890", "12345678901234567000")
            , ("0.000033333333333333335", "0.000033333333333333335")
            , ("-42.25", "-42.25")
            , ("100", "100")
            ]
      mapM_
        (\(input, expected) -> it (input ++ " -> " ++ expected) $ canonical input `shouldBe` expected)
        cases
    describe "objects" $ do
      it "sorts keys by UTF-16 code units" $
        canonical "{\"～\":1,\"😀\":2,\"a\":3,\"A\":4,\"é\":5}" `shouldBe` "{\"A\":4,\"a\":3,\"é\":5,\"😀\":2,\"～\":1}"
      it "emits no whitespace" $
        canonical "{ \"b\" : [ 1 , 2 ] , \"a\" : { \"c\" : null } }" `shouldBe` "{\"a\":{\"c\":null},\"b\":[1,2]}"
      it "keeps nested empty objects and arrays" $
        canonical "{\"a\":{},\"b\":[],\"c\":[{},[]]}" `shouldBe` "{\"a\":{},\"b\":[],\"c\":[{},[]]}"
      it "keeps null" $
        canonical "{\"a\":null}" `shouldBe` "{\"a\":null}"
      it "keeps booleans" $
        canonical "[true,false]" `shouldBe` "[true,false]"
    describe "strings" $ do
      it "escapes the short forms" $
        canonical "\"\\b\\t\\n\\f\\r\\\"\\\\\"" `shouldBe` "\"\\b\\t\\n\\f\\r\\\"\\\\\""
      it "escapes other control characters as lowercase \\u00xx" $
        canonical "\"\\u0000\\u001f\\u001F\"" `shouldBe` "\"\\u0000\\u001f\\u001f\""
      it "does not escape the slash" $
        canonical "\"/a/b\"" `shouldBe` "\"/a/b\""
      it "keeps non-ASCII characters literal in UTF-8" $
        canonical "\"\\u20ac \\ud83d\\ude00 é\"" `shouldBe` "\"€ 😀 é\""
      it "does not escape U+007F or U+2028" $
        canonical "\"\\u007f\\u2028\"" `shouldBe` "\"\DEL\x2028\""
    it "accepts a built Value" $
      toString (encodeCanonical (object ["b" .= Null, "a" .= (1 :: Int)])) `shouldBe` "{\"a\":1,\"b\":null}"

canonical :: String -> String
canonical input =
  case decode (BSL.fromStrict . TE.encodeUtf8 . T.pack $ input) :: Maybe Value of
    Just value -> toString (encodeCanonical value)
    Nothing -> error ("not valid JSON: " ++ input)

toString :: BSL.ByteString -> String
toString = T.unpack . TE.decodeUtf8 . BSL.toStrict
