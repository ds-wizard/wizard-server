module Specs.Api.Handler.Project.Detail_Content_PUT (
  detail_content_PUT,
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

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.ProjectContentChangeDTO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Project.ProjectVersionDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import Shared.Service.Project.Event.ProjectEventMapper
import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/projects/{projectUuid}/content
-- ------------------------------------------------------------------------
detail_content_PUT :: RequestContext -> SpecWith ((), Application)
detail_content_PUT requestContext =
  describe "PUT /wizard-api/projects/{projectUuid}/content" $ do
    test_200 requestContext
    test_400 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/content"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDto projectUuid =
  ProjectContentChangeDTO
    { events = [toEventChangeDTO (slble_rQ2' projectUuid)]
    }

reqBody projectUuid = encode (reqDto projectUuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" requestContext project1 project1EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Owner, VisibleView)" requestContext project2 project2EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, Public)" requestContext project3 project3EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing" requestContext project10 project10EventsEdited []

create_test_200 title requestContext project projectEventsEdited authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = reqDto project.uuid
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project7) requestContext
      runInContextIO (insertProjectEvents project7Events) requestContext
      runInContextIO (traverse_ insertProjectVersion project7Versions) requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO (insertProjectEvents project10Events) requestContext
      runInContextIO (traverse_ insertProjectVersion project10Versions) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBody project.uuid)
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectContentChangeDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectDtos resBody expDto
      -- AND: Find a result in DB
      assertExistenceOfProjectContentInDB requestContext project.uuid projectEventsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod (reqUrlT project3.uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    project1EventsEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    requestContext
    project2
    project2EventsEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)"
    requestContext
    project7
    project7EventsEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    requestContext
    project3
    project3EventsEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title requestContext project projectEventsEdited authHeader reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project7) requestContext
      runInContextIO (insertProject project10) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBody project.uuid)
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/content"
    (reqHeadersT [reqAuthHeader])
    (reqBody $ u' "f08ead5f-746d-411b-aee6-77ea3d24016a")
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
