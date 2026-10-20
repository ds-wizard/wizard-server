module Specs.Service.Mcp.Tool.MailTemplateLocaleTools.Common where

import qualified Data.List as L
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO
import qualified Shared.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration as MailTemplateLocale
import Shared.Model.MailTemplateLocale.MailTemplateLocale

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runMailTemplateLocaleMigrations requestContext = do
  runInContextIO MailTemplateLocale.runMigration requestContext
  runInContextIO MailTemplateLocale.runS3Migration requestContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertMailTemplateLocaleCodesInDB requestContext expCodes = do
  locales <- getOneFromDB findMailTemplateLocales requestContext
  liftIO $ L.sort (fmap (.code) locales) `shouldBe` L.sort expCodes
