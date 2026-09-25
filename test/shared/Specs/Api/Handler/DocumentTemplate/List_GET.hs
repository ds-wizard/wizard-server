module Specs.Api.Handler.DocumentTemplate.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Service.DocumentTemplate.WizardDocumentTemplateMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/document-templates
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/document-templates" $ do
    test_200 requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/document-templates"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/api/document-templates"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (query 'q')"
    requestContext
    "/api/document-templates?q=Project Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    requestContext
    "/api/document-templates?q=Non-existing Project Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO]))
  create_test_200
    "HTTP 200 OK (query 'templateId')"
    requestContext
    "/api/document-templates?templateId=project-report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (query 'templateId' for non-existing)"
    requestContext
    "/api/document-templates?templateId=non-existing-template"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO]))
  create_test_200
    "HTTP 200 OK (outdated=false)"
    requestContext
    "/api/document-templates?outdated=false"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (outdated=true)"
    requestContext
    "/api/document-templates?outdated=true"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO]))
  create_test_200_workspace
    "HTTP 200 OK (tenant=true - tenant rows only)"
    requestContext
    "/api/document-templates?tenant=true&sort=name,asc"
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200_workspace
    "HTTP 200 OK (w - tenant and workspace rows)"
    requestContext
    "/api/document-templates?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f&sort=name,asc"
    (Page "documentTemplates" (PageMetadata 20 2 1 0) [wizardDocumentTemplateSimpleDTO, workspaceWizardDocumentTemplateSimpleDTO])
  create_test_200_workspace
    "HTTP 200 OK (no parameter - every plane of the caller)"
    requestContext
    "/api/document-templates?sort=name,asc"
    (Page "documentTemplates" (PageMetadata 20 2 1 0) [wizardDocumentTemplateSimpleDTO, workspaceWizardDocumentTemplateSimpleDTO])
  create_test_200_workspace
    "HTTP 200 OK (foreign w - empty page)"
    requestContext
    "/api/document-templates?w=0b6e4d3c-2a1f-4e8d-8c7b-9a5f4e3d2c1b&sort=name,asc"
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO]))

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
      runInContextIO DT_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

create_test_200_workspace title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      runInContextIO R_Migration.runMigration requestContext
      runInContextIO (insertDocumentTemplate workspaceWizardDocumentTemplate) requestContext
      enableMultiWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

workspaceWizardDocumentTemplateSimpleDTO :: DocumentTemplateSimpleDTO
workspaceWizardDocumentTemplateSimpleDTO =
  toSimpleDTO' True (toList workspaceWizardDocumentTemplate Nothing (Just globalRegistryOrganization) ReleasedDocumentTemplatePhase)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
