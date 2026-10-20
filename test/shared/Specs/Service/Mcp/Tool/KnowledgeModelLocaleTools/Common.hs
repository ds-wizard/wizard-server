module Specs.Service.Mcp.Tool.KnowledgeModelLocaleTools.Common where

import Control.Monad (void)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.S3.KnowledgeModel.KnowledgeModelLocaleS3
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelPackageTools.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runLocaleMigrations requestContext = do
  runPackageMigrations requestContext
  runInContextIO (insertKnowledgeModelLocale czechGlobalKmLocale) requestContext
  void $ runInContextIO (putKnowledgeModelLocale czechGlobalKmLocale.uuid translationPoFileName czechPoContent) requestContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertLocaleCodesInDB :: RequestContext -> [String] -> WaiSession () ()
assertLocaleCodesInDB requestContext expCodes = do
  locales <- getOneFromDB (findKnowledgeModelLocalesByPackageUuid czechGlobalKmLocale.knowledgeModelPackageUuid) requestContext
  liftIO $ fmap (.code) locales `shouldMatchList` expCodes
