module Specs.Api.Handler.Document.Detail_Available_Submission_Services_GET (
  detail_available_submission_Services_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Settings.SettingsSubmissionDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.Settings.SettingsMigration
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Model.Document.Document
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Service.Submission.SubmissionService (toSubmissionServiceSimple)
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------------------
-- GET /api/documents/{docUuid}/available-submission-services
-- ------------------------------------------------------------------------------------
detail_available_submission_Services_GET :: RequestContext -> SpecWith ((), Application)
detail_available_submission_Services_GET requestContext =
  describe "GET /api/documents/{docUuid}/available-submission-services" $ do
    test_200 requestContext
    test_200_format_mismatch requestContext
    test_200_workspace_override requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/documents/264ca352-1a99-4ffd-860e-32aee9a98428/available-submission-services"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" requestContext project1 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" requestContext project3 [reqNonAdminAuthHeader]

create_test_200 title requestContext project authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = [toSubmissionServiceSimple settingsSubmissionService]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      runInContextIO (deleteDocumentByUuid doc1.uuid) requestContext
      runInContextIO (insertDocument (doc1 {projectUuid = Just project.uuid})) requestContext
      runInContextIO seedSettingsSubmissionService requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_200_format_mismatch requestContext =
  it "HTTP 200 OK (service whose supported format names another format is not offered)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let format = settingsSubmissionServiceSupportedFormat {formatName = "Other format"} :: SettingsSubmissionServiceSupportedFormat
    let service = settingsSubmissionService {supportedFormats = [format]} :: SettingsSubmissionService
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    runInContextIO TML_Migration.runMigration requestContext
    runInContextIO PRJ_Migration.runMigration requestContext
    runInContextIO DOC_Migration.runMigration requestContext
    runInContextIO (saveSettingsSubmission defaultTenantUuid Nothing (settingsSubmission {services = [service]} :: SettingsSubmission)) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals (encode ([] :: [SettingsSubmissionServiceSimple]))}
    response `shouldRespondWith` responseMatcher

test_200_workspace_override requestContext =
  it "HTTP 200 OK (the submission override of the document's workspace replaces the organization services)" $ do
    -- GIVEN: Prepare expectation
    let expHeaders = resCtHeader : resCorsHeaders
    let workspaceService = settingsSubmissionService {sId = "workspaceSubmissionServer", name = "Workspace Submission Server"} :: SettingsSubmissionService
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration requestContext
    runInContextIO TML_Migration.runMigration requestContext
    runInContextIO PRJ_Migration.runMigration requestContext
    runInContextIO DOC_Migration.runMigration requestContext
    runInContextIO seedSettingsSubmissionService requestContext
    enableMultiWorkspace requestContext
    runInContextIO (saveSettingsSubmission defaultTenantUuid (Just defaultWorkspaceUuid) (settingsSubmission {services = [workspaceService]} :: SettingsSubmission)) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = 200, matchBody = bodyEquals (encode [toSubmissionServiceSimple workspaceService])}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    requestContext
    project2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")

create_test_403 title requestContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO DOC_Migration.runMigration requestContext
      runInContextIO (insertProject project7) requestContext
      runInContextIO (deleteDocumentByUuid doc1.uuid) requestContext
      runInContextIO (insertDocument (doc1 {projectUuid = Just project.uuid})) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/documents/dc9fe65f-748b-47ec-b30c-d255bbac64a0/available-submission-services"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "document"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
