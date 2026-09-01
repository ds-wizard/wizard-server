module Specs.Api.Handler.DocumentTemplate.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateLocales
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/document-templates/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: RequestContext -> SpecWith ((), Application)
list_suggestions_GET requestContext =
  describe "GET /wizard-api/document-templates/suggestions" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/document-templates/suggestions"

reqHeadersT reqAuthHeader = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/wizard-api/document-templates/suggestions"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [toSuggestionDTO' wizardDocumentTemplate wizardDocumentTemplateFormats [czechWizardDocumentTemplateLocaleList]])
  create_test_200
    "HTTP 200 OK (query 'q')"
    requestContext
    "/wizard-api/document-templates/suggestions?q=Project Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [toSuggestionDTO' wizardDocumentTemplate wizardDocumentTemplateFormats [czechWizardDocumentTemplateLocaleList]])
  create_test_200
    "HTTP 200 OK (query 'knowledgeModelPackageUuid')"
    requestContext
    (BS.pack $ "/wizard-api/document-templates/suggestions?knowledgeModelPackageUuid=" ++ U.toString globalKmPackage.uuid)
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [toSuggestionDTO' wizardDocumentTemplate wizardDocumentTemplateFormats [czechWizardDocumentTemplateLocaleList]])
  create_test_200
    "HTTP 200 OK (query 'knowledgeModelPackageUuid' - no templates)"
    requestContext
    (BS.pack $ "/wizard-api/document-templates/suggestions?knowledgeModelPackageUuid=" ++ U.toString netherlandsKmPackage.uuid)
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSuggestionDTO]))
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    requestContext
    "/wizard-api/document-templates/suggestions?q=Non-existing Project Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSuggestionDTO]))

create_test_200 title requestContext reqUrl reqAuthHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO DT_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      runInContextIO (updateDocumentTemplateById $ wizardDocumentTemplate {allowedPackages = [kmPackagePatternAllEdited]}) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
