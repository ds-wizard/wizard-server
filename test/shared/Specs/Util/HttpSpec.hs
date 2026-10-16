module Specs.Util.HttpSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Util.Http

httpSpec =
  describe "Http" $
    describe "maskQueryString" $ do
      it "masks the Authorization parameter" $
        maskQueryString "?Authorization=Bearer%20eyJhbGciOi.x.y" `shouldBe` "?Authorization=***"
      it "keeps the other parameters" $
        maskQueryString "?q=abc&Authorization=Bearer%20token&page=2" `shouldBe` "?q=abc&Authorization=***&page=2"
      it "keeps a query without Authorization" $
        maskQueryString "?q=abc&size=10" `shouldBe` "?q=abc&size=10"
      it "keeps an empty query" $
        maskQueryString "" `shouldBe` ""
