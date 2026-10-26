module Specs.Util.HttpSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Util.Http

httpSpec =
  describe "Http" $ do
    describe "attachmentContentDisposition" $ do
      it "keeps an ASCII name" $
        attachmentContentDisposition "report.pdf" `shouldBe` "attachment; filename=\"report.pdf\"; filename*=UTF-8''report.pdf"
      it "keeps a non-ASCII name only in the percent-encoded filename*" $
        attachmentContentDisposition "Zoë.html" `shouldBe` "attachment; filename=\"Zo.html\"; filename*=UTF-8''Zo%C3%AB.html"
      it "drops quotes from the fallback name" $
        attachmentContentDisposition " my \"final\" report.pdf " `shouldBe` "attachment; filename=\"my final report.pdf\"; filename*=UTF-8''my%20%22final%22%20report.pdf"
    describe "maskQueryString" $ do
      it "masks the Authorization parameter" $
        maskQueryString "?Authorization=Bearer%20eyJhbGciOi.x.y" `shouldBe` "?Authorization=***"
      it "keeps the other parameters" $
        maskQueryString "?q=abc&Authorization=Bearer%20token&page=2" `shouldBe` "?q=abc&Authorization=***&page=2"
      it "keeps a query without Authorization" $
        maskQueryString "?q=abc&size=10" `shouldBe` "?q=abc&size=10"
      it "keeps an empty query" $
        maskQueryString "" `shouldBe` ""
