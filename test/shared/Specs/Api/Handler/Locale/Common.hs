module Specs.Api.Handler.Locale.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Model.Locale.Locale
import Shared.S3.Locale.LocaleS3

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runLocaleEnMigration requestContext = do
  runInContextIO (insertLocale localeEn) requestContext
  runInContextIO (putLocaleJson localeEn.uuid (toJedContent localeEnPo)) requestContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfLocaleInDB requestContext locale = do
  localeFromDb <- getOneFromDB (findLocaleByUuid locale.uuid) requestContext
  compareLocaleDtos localeFromDb locale

assertLocaleContentInS3 requestContext locale po = do
  poFromS3 <- getOneFromDB (retrieveLocalePo locale.uuid) requestContext
  liftIO $ poFromS3 `shouldBe` po
  jsonFromS3 <- getOneFromDB (retrieveLocaleJsonWithTenant locale.tenantUuid locale.uuid) requestContext
  liftIO $ jsonFromS3 `shouldBe` toJedContent po

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareLocaleDtos resDto expDto = do
  liftIO $ resDto.code `shouldBe` expDto.code
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.potVersion `shouldBe` expDto.potVersion
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
  liftIO $ resDto.defaultLocale `shouldBe` expDto.defaultLocale
  liftIO $ resDto.official `shouldBe` expDto.official
