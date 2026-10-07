module Specs.Service.Locale.Registry.RegistryLocaleMapperSpec where

import Data.Time
import Test.Hspec

import Shared.Api.Resource.Locale.Registry.RegistryLocaleDTO
import Shared.Model.Registry.RegistryLocale
import Shared.Model.Registry.RegistryMailTemplateLocale
import Shared.Service.Locale.Registry.RegistryLocaleMapper

registryLocaleMapperSpec =
  describe "RegistryLocaleMapper" $ do
    let createdAt = UTCTime (fromGregorian 2026 1 1) 0
    it "marks a UI locale installed by the normalized code" $ do
      let locale = RegistryLocale {code = "pt-BR", name = "Português", remoteVersion = "5.0.0", createdAt = createdAt}
      (toRegistryLocaleDTO ["pt_br"] locale).installed `shouldBe` True
      (toRegistryLocaleDTO ["pt"] locale).installed `shouldBe` False
    it "marks a mail template locale installed by the normalized code" $ do
      let locale = RegistryMailTemplateLocale {code = "zh_hant", name = "中文", remoteVersion = "5.0.0", createdAt = createdAt}
      (toRegistryMailTemplateLocaleDTO ["zh-Hant"] locale).installed `shouldBe` True
