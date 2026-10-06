module Specs.Api.Handler.Document.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Model.Workspace.Workspace
import Shared.Service.Document.DocumentMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/documents
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/documents" $ do
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

reqUrl = "/api/documents"

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/api/documents"
    ( Page
        "documents"
        (PageMetadata 20 3 1 0)
        [ toDTOWithDocTemplate doc1 project1 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc3 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        ]
    )
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    "/api/documents?q=My exported document 2"
    (Page "documents" (PageMetadata 20 1 1 0) [toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    requestContext
    "/api/documents?q=Non-existing document"
    (Page "documents" (PageMetadata 20 0 0 0) ([] :: [DocumentDTO]))
  create_test_200
    "HTTP 200 OK (documentTemplateUuid)"
    requestContext
    "/api/documents?documentTemplateUuid=557e76d8-338a-4664-886a-f6af2228776c&sort=name,asc"
    ( Page
        "documents"
        (PageMetadata 20 3 1 0)
        [ toDTOWithDocTemplate doc1 project1 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc3 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        ]
    )

create_test_200 title requestContext reqUrl expDto =
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
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO DOC_Migration.runMigration requestContext
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
    "HTTP 200 OK (all workspaces of the caller)"
    requestContext
    insertSecondWorkspace
    "/api/documents?sort=name,asc"
    ( Page
        "documents"
        (PageMetadata 20 4 1 0)
        [ toDTOWithDocTemplate doc1 project1 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc3 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , secondWorkspaceDocDto
        ]
    )
  create_test_200_workspace
    "HTTP 200 OK (w)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/documents?w=" ++ U.toString secondWorkspace.uuid)
    (Page "documents" (PageMetadata 20 1 1 0) [secondWorkspaceDocDto])
  create_test_200_workspace
    "HTTP 200 OK (workspace of another tenant)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/documents?w=" ++ U.toString differentWorkspaceUuid)
    (Page "documents" (PageMetadata 20 0 0 0) ([] :: [DocumentDTO]))
  create_test_200_workspace
    "HTTP 200 OK (only the workspaces the caller is a member of)"
    requestContext
    insertSecondWorkspaceWithoutCaller
    "/api/documents?sort=name,asc"
    ( Page
        "documents"
        (PageMetadata 20 3 1 0)
        [ toDTOWithDocTemplate doc1 project1 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        , toDTOWithDocTemplate doc3 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple
        ]
    )

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
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      runInContextIO (insertPackage amsterdamKmPackage) requestContext
      prepareWorkspace requestContext
      runInContextIO (insertProject project16) requestContext
      runInContextIO (insertDocument secondWorkspaceDoc) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

secondWorkspaceDocDto :: DocumentDTO
secondWorkspaceDocDto = toDTOWithDocTemplate secondWorkspaceDoc project16 Nothing [] wizardDocumentTemplate formatJsonSimple

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST (tenant=true on a workspace-only entity)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED)
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/documents?tenant=true" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN - only 'admin' can view" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqNonAdminAuthHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError (_ERROR_VALIDATION__FORBIDDEN "Missing permission: projects.edit")
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_403_not_member requestContext =
  createNotMemberTest requestContext (runInContextIO U_Migration.runMigration requestContext) reqMethod (BS.pack $ "/api/documents?w=" ++ U.toString secondWorkspaceUuid) reqBody "projects.edit"
