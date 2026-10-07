module Specs.Api.Handler.MailTemplateLocale.Common where

import Control.Monad (void)
import qualified Data.List as L
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import Shared.Database.DAO.TemporaryFile.TemporaryFileDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigMailDAO
import Shared.Model.MailTemplateLocale.MailTemplateLocale
import Shared.Model.TemporaryFile.TemporaryFile
import Shared.Model.Tenant.Config.TenantConfigMail
import Shared.Model.Tenant.Config.TenantConfigMailDM
import Shared.S3.Common
import Shared.S3.MailTemplateLocale.MailTemplateLocaleS3
import Shared.S3.TemporaryFile.TemporaryFileS3

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runMailTemplatePotMigration requestContext customTemplates mDefaultPot mCustomPot = do
  runInContextIO deleteTenantConfigMails requestContext
  runInContextIO (insertTenantConfigMail (defaultMail {tenantUuid = defaultTenantUuid, customTemplates = customTemplates} :: TenantConfigMail)) requestContext
  runInContextIO (setObject defaultPotObject mDefaultPot) requestContext
  runInContextIO (setObject customPotObject mCustomPot) requestContext
  where
    setObject object (Just content) = void $ createPutObjectFn object Nothing Nothing content
    setObject object Nothing = createRemoveObjectFn object

runMailTemplateLocaleMoMigration requestContext locale =
  runInContextIO (createPutObjectFn (mailTemplateLocalePath locale.uuid moFileName) Nothing Nothing "compiled") requestContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfMailTemplateLocaleInDB requestContext locale = do
  localeFromDb <- getOneFromDB (findMailTemplateLocaleByUuid locale.uuid) requestContext
  compareMailTemplateLocaleDtos localeFromDb locale

assertMailTemplateLocaleContentInS3 requestContext locale po = do
  poFromS3 <- getOneFromDB (retrieveMailTemplateLocalePo locale.uuid) requestContext
  liftIO $ poFromS3 `shouldBe` po

assertAbsenceOfMailTemplateLocaleMoInS3 requestContext locale = do
  mo <- getOneFromDB (createFindObjectFn (mailTemplateLocalePath locale.uuid moFileName)) requestContext
  liftIO $ mo `shouldBe` Nothing

assertTemporaryFileContentInS3 requestContext resDto content = do
  temporaryFiles <- getOneFromDB findTemporaryFiles requestContext
  let [temporaryFile] = filter (\file -> U.toString file.uuid `L.isInfixOf` resDto.url) temporaryFiles
  contentFromS3 <- getOneFromDB (retrieveTemporaryFile temporaryFile.uuid temporaryFile.fileName) requestContext
  liftIO $ contentFromS3 `shouldBe` content

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareMailTemplateLocaleDtos resDto expDto = do
  liftIO $ resDto.code `shouldBe` expDto.code
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.potVersion `shouldBe` expDto.potVersion
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
  liftIO $ resDto.defaultLocale `shouldBe` expDto.defaultLocale
  liftIO $ resDto.official `shouldBe` expDto.official
