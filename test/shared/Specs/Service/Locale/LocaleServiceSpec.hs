module Specs.Service.Locale.LocaleServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.Migration.Development.Locale.Data.Locales
import qualified Shared.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Shared.Model.Locale.Locale
import Shared.Service.Locale.LocaleService

import Specs.Common

localeServiceIntegrationSpec requestContext =
  describe "Locale Service Integration" $
    describe "determineLocale" $ do
      it "Matches the base language of a preferred region" $ do
        -- GIVEN: Prepare expectation
        let expectation = Right (Just localeDe.uuid)
        -- AND: Run migrations
        runInContextIO LOC_Migration.runMigration requestContext
        -- WHEN:
        result <- runInContext (determineLocale (Just "de-DE,de;q=0.9")) requestContext
        -- THEN:
        result `shouldBe` expectation
      it "Skips disabled locales and returns nothing without a match" $ do
        -- GIVEN: Prepare expectation
        let expectation = Right Nothing
        -- AND: Run migrations
        runInContextIO LOC_Migration.runMigration requestContext
        -- WHEN:
        result <- runInContext (determineLocale (Just "cs-CZ,fr;q=0.8")) requestContext
        -- THEN:
        result `shouldBe` expectation
      it "Returns nothing without the header" $ do
        -- GIVEN: Prepare expectation
        let expectation = Right Nothing
        -- AND: Run migrations
        runInContextIO LOC_Migration.runMigration requestContext
        -- WHEN:
        result <- runInContext (determineLocale Nothing) requestContext
        -- THEN:
        result `shouldBe` expectation
