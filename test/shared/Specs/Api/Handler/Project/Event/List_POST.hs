module Specs.Api.Handler.Project.Event.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.Project.Data.ProjectEventBatches
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectUtil
import Shared.Service.Project.Event.ProjectEventMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/projects/{projectUuid}/events
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/projects/{projectUuid}/events" $ do
    test_200 requestContext
    test_400 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT projectUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid ++ "/events"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDto = toEventChangeDTO (sre_rQ1Updated' project10Uuid)

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" requestContext project1 project1Events [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" requestContext project3 project3Events [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing)" requestContext project10 project10Events []

create_test_200 title requestContext project projectEvents authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 204
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO deleteProjects requestContext
      runInContextIO (insertProject project) requestContext
      runInContextIO (insertProjectEvents projectEvents) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST if a path doesn't match the knowledge model" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project1.uuid
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let invalidPath = createReplyKey [chapter1.uuid, question3.uuid]
      let reqBody = encode (SetReplyEventChangeDTO' (srche_rQ2 {path = invalidPath} :: SetReplyEventChangeDTO))
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "path" [_ERROR_VALIDATION__PROJECT_EVENT_PATH_ABSENCE invalidPath])
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertExistenceOfProjectContentInDB requestContext project1.uuid project1Events

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" requestContext project1 project1Events [reqNonAdminAuthHeader] (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" requestContext project2 project2Events [reqNonAdminAuthHeader] (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403 "HTTP 403 FORBIDDEN (Commenter)" requestContext (project13 {visibility = PrivateProjectVisibility}) project13Events [reqNonAdminAuthHeader] (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403 "HTTP 403 FORBIDDEN (Non-Commenter, VisibleComment)" requestContext project13 project13Events [reqIsaacAuthTokenHeader] (_ERROR_VALIDATION__FORBIDDEN "Edit Project")
  create_test_403 "HTTP 403 FORBIDDEN (Anonymous, VisibleComment, AnyoneWithLinkComment)" requestContext (project13 {sharing = AnyoneWithLinkCommentProjectSharing}) project13Events [] _ERROR_SERVICE_USER__MISSING_USER
  create_test_403 "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)" requestContext project7 project7Events [] _ERROR_SERVICE_USER__MISSING_USER
  create_test_403 "HTTP 403 FORBIDDEN (Anonymous, VisibleView)" requestContext project2 project2Events [] _ERROR_SERVICE_USER__MISSING_USER
  create_test_403 "HTTP 403 FORBIDDEN (Anonymous, Public)" requestContext project3 project3Events [] _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title requestContext project projectEvents authHeader errorMessage =
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
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO deleteProjects requestContext
      runInContextIO (insertProject project) requestContext
      runInContextIO (insertProjectEvents projectEvents) requestContext
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
    "/api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/events"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
