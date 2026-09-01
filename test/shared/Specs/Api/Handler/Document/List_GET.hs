module Specs.Api.Handler.Document.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Service.Document.DocumentMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/documents
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /wizard-api/documents" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/documents"

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    "/wizard-api/documents"
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
    "/wizard-api/documents?q=My exported document 2"
    (Page "documents" (PageMetadata 20 1 1 0) [toDTOWithDocTemplate doc2 project2 (Just "Version 1") [] wizardDocumentTemplate formatJsonSimple])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    requestContext
    "/wizard-api/documents?q=Non-existing document"
    (Page "documents" (PageMetadata 20 0 0 0) ([] :: [DocumentDTO]))
  create_test_200
    "HTTP 200 OK (documentTemplateUuid)"
    requestContext
    "/wizard-api/documents?documentTemplateUuid=557e76d8-338a-4664-886a-f6af2228776c&sort=name,asc"
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
      let expDto = ForbiddenError (_ERROR_VALIDATION__FORBIDDEN "Missing permission: ProjectsEditRolePermission")
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
