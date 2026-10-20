module Specs.Service.Mcp.Tool.DocumentTemplateLocaleTools.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateLocaleDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale
import Shared.S3.DocumentTemplate.DocumentTemplateS3
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runLocaleMigrations requestContext = do
  runInContextIO DocumentTemplate.runMigration requestContext
  runInContextIO DocumentTemplate.runS3Migration requestContext

runPotFileMigrations requestContext = do
  runLocaleMigrations requestContext
  runInContextIO (updateDocumentTemplatePotFileReadyByUuid wizardDocumentTemplate.uuid True) requestContext
  let fileName = potFileName wizardDocumentTemplate.id wizardDocumentTemplate.version
  runInContextIO (putPotFile wizardDocumentTemplate.uuid fileName wizardDocumentTemplatePotContent) requestContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertLocaleCodesInDB :: RequestContext -> [String] -> WaiSession () ()
assertLocaleCodesInDB requestContext expCodes = do
  locales <- getOneFromDB (findDocumentTemplateLocalesByDocumentTemplateUuid wizardDocumentTemplate.uuid) requestContext
  liftIO $ fmap (.code) locales `shouldMatchList` expCodes
