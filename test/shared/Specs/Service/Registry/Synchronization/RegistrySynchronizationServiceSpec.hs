module Specs.Service.Registry.Synchronization.RegistrySynchronizationServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.DAO.Registry.RegistryLocaleDAO
import Shared.Database.DAO.Registry.RegistryMailTemplateLocaleDAO
import Shared.Database.Migration.Development.Registry.Data.RegistryLocales
import Shared.Model.Registry.RegistryLocale
import Shared.Model.Registry.RegistryMailTemplateLocale
import Shared.Service.Registry.Synchronization.RegistrySynchronizationService

import Specs.Common
import Specs.Integration.Http.Registry.Common

registrySynchronizationServiceSpec requestContext =
  describe "Registry Synchronization Service" $
    it "fills the locale and the mail template locale caches with the versions offered for the instance version" $ do
      -- GIVEN: Prepare expectation
      let expLocales = fmap (\l -> (l.code, l.name, l.remoteVersion)) [deRegistryLocale, nlRegistryLocale]
      let expMailTemplateLocales = fmap (\l -> (l.code, l.name, l.remoteVersion)) [deRegistryMailTemplateLocale]
      -- AND: A registry that offers locales for the instance version only
      context <- withFakeRegistry requestContext
      -- WHEN:
      result <- runInContext synchronizeData context
      -- THEN:
      result `shouldBe` Right ()
      Right locales <- runInContext findRegistryLocales context
      fmap (\l -> (l.code, l.name, l.remoteVersion)) locales `shouldBe` expLocales
      Right mailTemplateLocales <- runInContext findRegistryMailTemplateLocales context
      fmap (\l -> (l.code, l.name, l.remoteVersion)) mailTemplateLocales `shouldBe` expMailTemplateLocales
