module Specs.Api.Handler.DocumentTemplateDraft.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import qualified Shared.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.Error.Error
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/document-template-drafts
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/document-template-drafts" $ do
    test_200 requestContext
    test_200_workspace requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_not_member requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/document-template-drafts"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/api/document-template-drafts"
    reqAuthHeader
    (Page "documentTemplateDrafts" (PageMetadata 20 1 1 0) [toDraftList wizardDocumentTemplateDraft])
  create_test_200
    "HTTP 200 OK (query 'q')"
    requestContext
    "/api/document-template-drafts?q=Project Report"
    reqAuthHeader
    (Page "documentTemplateDrafts" (PageMetadata 20 1 1 0) [toDraftList wizardDocumentTemplateDraft])
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    requestContext
    "/api/document-template-drafts?q=Non-existing Project Report"
    reqAuthHeader
    (Page "documentTemplateDrafts" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateDraftList]))

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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_workspace requestContext = do
  create_test_200_workspace
    "HTTP 200 OK (tenant=true - tenant-plane drafts only)"
    requestContext
    insertSecondWorkspace
    "/api/document-template-drafts?tenant=true&sort=name,asc"
    (Page "documentTemplateDrafts" (PageMetadata 20 1 1 0) [toDraftList wizardDocumentTemplateDraft])
  create_test_200_workspace
    "HTTP 200 OK (w - tenant and workspace drafts)"
    requestContext
    insertSecondWorkspace
    "/api/document-template-drafts?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9&sort=name,asc"
    (Page "documentTemplateDrafts" (PageMetadata 20 2 1 0) [toDraftList wizardDocumentTemplateDraft, toDraftList secondWorkspaceDraft])

create_test_200_workspace title requestContext prepareWorkspace reqUrl expDto =
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
      prepareWorkspace requestContext
      runInContextIO (insertDocumentTemplate secondWorkspaceDraft) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

secondWorkspaceDraft :: DocumentTemplate
secondWorkspaceDraft =
  wizardDocumentTemplateDraft
    { uuid = u' "e4f5a6b7-c8d9-4e0f-a1b2-c3d4e5f6a7b8"
    , name = "DRAFT: Second Workspace Report"
    , id = "global.second-workspace-report"
    , workspaceUuid = Just secondWorkspaceUuid
    }

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST (w and tenant=true)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__SCOPE_CONFLICT)
      -- AND: Run migrations
      runInContextIO DT_Migration.runMigration requestContext
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/document-template-drafts?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9&tenant=true" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [] reqBody "documentTemplates.useEditor"

test_403_not_member requestContext =
  createNotMemberTest requestContext (runInContextIO DT_Migration.runMigration requestContext) reqMethod "/api/document-template-drafts?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9&sort=name,asc" reqBody "documentTemplates.useEditor"
