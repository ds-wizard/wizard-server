module Specs.Util.LocaleCodeSpec where

import Test.Hspec

import Shared.Util.LocaleCode

localeCodeSpec =
  describe "LocaleCode" $ do
    describe "normalizeLocaleCode" $
      it "normalizes to BCP 47 casing" $ do
        normalizeLocaleCode "pt_br" `shouldBe` "pt-BR"
        normalizeLocaleCode "PT-br" `shouldBe` "pt-BR"
        normalizeLocaleCode " de " `shouldBe` "de"
        normalizeLocaleCode "zh_hant_tw" `shouldBe` "zh-Hant-TW"
        normalizeLocaleCode "es-419" `shouldBe` "es-419"
    describe "isValidLocaleCode" $ do
      it "accepts a language with optional subtags" $ do
        isValidLocaleCode "de" `shouldBe` True
        isValidLocaleCode "pt-BR" `shouldBe` True
        isValidLocaleCode "zh-Hant-TW" `shouldBe` True
        isValidLocaleCode "es-419" `shouldBe` True
        isValidLocaleCode "fil" `shouldBe` True
      it "rejects empty, malformed and non-ASCII codes" $ do
        isValidLocaleCode "" `shouldBe` False
        isValidLocaleCode "d" `shouldBe` False
        isValidLocaleCode "deutsch" `shouldBe` False
        isValidLocaleCode "12" `shouldBe` False
        isValidLocaleCode "de-" `shouldBe` False
        isValidLocaleCode "de-123456789" `shouldBe` False
        isValidLocaleCode "de-\252" `shouldBe` False
        isValidLocaleCode "de AT" `shouldBe` False
    describe "matchLocaleCode" $ do
      let locales = [("de", 1), ("pt-BR", 2), ("en", 3)]
      it "matches exactly first" $ do
        matchLocaleCode "pt_br" locales `shouldBe` Just 2
        matchLocaleCode "DE" locales `shouldBe` Just 1
      it "falls back to the primary subtag of the requested code" $ do
        matchLocaleCode "de-AT" locales `shouldBe` Just 1
        matchLocaleCode "pt" locales `shouldBe` Nothing
        matchLocaleCode "cs" locales `shouldBe` Nothing
    describe "parseAcceptLanguage" $ do
      it "sorts by quality and drops zero and empty items" $
        parseAcceptLanguage "en;q=0.5, de-DE,de;q=0.9, fr;q=0, ," `shouldBe` ["de-DE", "de", "en"]
      it "keeps the header order for equal quality" $
        parseAcceptLanguage "cs, sk" `shouldBe` ["cs", "sk"]
    describe "matchAcceptLanguage" $ do
      let locales = [("de", "de"), ("cs", "cs")]
      it "takes the first preference that matches" $ do
        matchAcceptLanguage "de-DE,de;q=0.9" locales `shouldBe` Just "de"
        matchAcceptLanguage "fr, cs;q=0.5" locales `shouldBe` Just "cs"
      it "returns Nothing when nothing matches" $
        matchAcceptLanguage "fr, en;q=0.5" locales `shouldBe` Nothing
