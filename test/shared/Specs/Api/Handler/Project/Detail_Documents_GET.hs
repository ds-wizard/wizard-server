module Specs.Api.Handler.Project.Detail_Documents_GET (
  detail_documents_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Document.DocumentJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Common.Lens
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Document.Document
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.User.User
import Shared.S3.Document.DocumentS3
import Shared.Service.Document.DocumentMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/projects/{projectUuid}/documents
-- ------------------------------------------------------------------------
detail_documents_GET :: RequestContext -> SpecWith ((), Application)
detail_documents_GET requestContext =
  describe "GET /wizard-api/projects/{projectUuid}/documents" $ do
    test_200 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/documents?sort=name,asc"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 CREATED (Owner)" requestContext [reqAuthHeader]
  create_test_200 "HTTP 200 CREATED (Non-Owner)" requestContext [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 CREATED (Anonymous)" requestContext []

create_test_200 title requestContext authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project6.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      let doc1' = doc1 {projectUuid = Just project6.uuid, projectEventUuid = Just . getUuid $ slble_rQ1' project6.uuid}
      let doc2' = doc2 {projectUuid = Just project6.uuid, projectEventUuid = Just . getUuid $ slble_rQ1' project6.uuid, createdBy = Just userIsaac.uuid}
      runInContextIO U_Migration.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ_Migration.runMigration requestContext
      runInContextIO (insertProject project6) requestContext
      runInContextIO (insertProjectEvents project6Events) requestContext
      runInContextIO (traverse_ insertProjectVersion project6Versions) requestContext
      runInContextIO deleteDocuments requestContext
      runInContextIO removeDocumentContents requestContext
      runInContextIO (insertDocument doc1') requestContext
      runInContextIO (insertDocument doc2') requestContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            Page
              "documents"
              (PageMetadata 20 2 1 0)
              [ toDTOWithDocTemplate doc1' project6 (Just "Version 1") []
              , toDTOWithDocTemplate doc2' project6 (Just "Version 1") []
              ]
      let expBody = encode (fmap (\x -> x wizardDocumentTemplate formatJsonSimple) expDto)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    requestContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleEdit)"
    requestContext
    project3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title requestContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML_Migration.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/documents"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
