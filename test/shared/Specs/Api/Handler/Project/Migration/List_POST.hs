module Specs.Api.Handler.Project.Migration.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Shared.Api.Resource.Project.Migration.ProjectMigrationCreateJM ()
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectContent
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/projects/{projectUuid}/migrations
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /wizard-api/projects/{projectUuid}/migrations" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/migrations"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDto = projectMigrationCreateDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" requestContext project4 project4Events
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleView)" requestContext project4VisibleView project4VisibleViewEvents
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" requestContext project4VisibleEdit project4VisibleEditEvents

create_test_200 title requestContext project projectEvents =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCorsHeadersPlain
      -- AND: Prepare database
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertProject project) requestContext
      runInContextIO (insertProjectEvents projectEvents) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDetailQuestionnaireDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.uuid `shouldBe` project.uuid
      liftIO $ resBody.knowledgeModelPackage.uuid `shouldBe` netherlandsKmPackageV2.uuid
      liftIO $ resBody.knowledgeModel `shouldBe` km1NetherlandsV2
      liftIO $ resBody.phaseUuid `shouldBe` project4Ctn.phaseUuid
      liftIO $ resBody.replies `shouldBe` M.empty
      -- AND: Find a result in DB
      assertCountInDB findProjects requestContext 1
      eProjectFromDb <- runInContextIO (findProjectByUuid project.uuid) requestContext
      liftIO $ isRight eProjectFromDb `shouldBe` True
      let (Right projectFromDb) = eProjectFromDb
      liftIO $ projectFromDb.knowledgeModelPackageUuid `shouldBe` netherlandsKmPackageV2.uuid
      liftIO $ projectFromDb.selectedQuestionTagUuids `shouldBe` []
      liftIO $ projectFromDb.squashed `shouldBe` False
      eProjectEventsFromDb <- runInContextIO (findProjectEventsByProjectUuid project.uuid) requestContext
      liftIO $ isRight eProjectEventsFromDb `shouldBe` True
      let (Right projectEventsFromDb) = eProjectEventsFromDb
      liftIO $ projectEventsFromDb `shouldBe` projectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod (reqUrlT project4.uuid) "targetKnowledgeModelPackageUuid"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod (reqUrlT project4.uuid) [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" requestContext project1 "Migrate Project"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" requestContext project2 "Migrate Project"

create_test_403 title requestContext project reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT reqNonAdminAuthHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
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
    (reqUrlT project4.uuid)
    (reqHeadersT reqAuthHeader)
    reqBody
    "project"
    [("uuid", "57250a07-a663-4ff3-ac1f-16530f2c1bfe")]
