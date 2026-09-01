module Specs.Api.Handler.Project.Detail_Share_PUT (
  detail_share_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.ProjectShareChangeDTO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Service.Project.ProjectMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/projects/{projectUuid}/share
-- ------------------------------------------------------------------------
detail_share_PUT :: RequestContext -> SpecWith ((), Application)
detail_share_PUT requestContext =
  describe "PUT /wizard-api/projects/{projectUuid}/share" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT projectUuid = BS.pack $ "/wizard-api/projects/" ++ U.toString projectUuid ++ "/share"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT project =
  ProjectShareChangeDTO
    { visibility = project.visibility
    , sharing = project.sharing
    , permissions = fmap toProjectPermChangeDTO project.permissions
    }

reqBodyT project = encode $ reqDtoT project

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    requestContext
    project1
    project1ShareEdited
    project1Events
    project1Ctn
    []
    True
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Owner, VisibleView)"
    requestContext
    project2
    project2ShareEdited
    project2Events
    project2Ctn
    []
    False
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Non-Owner, Private, Sharing, Anonymous Enabled)"
    requestContext
    project10
    project10EditedShare
    project10Events
    project10Ctn
    [project10NikolaEditProjectPermDto]
    False
    [reqNonAdminAuthHeader]
    True

create_test_200 title requestContext project projectEdited projectEvents projectContent permissions showComments authHeader anonymousEnabled =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT projectEdited
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = reqDtoT projectEdited
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
      runInContextIO (insertProjectEvents project10Events) requestContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing requestContext anonymousEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfProjectInDB requestContext projectEdited projectEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = createInvalidJsonTest reqMethod (reqUrlT project3.uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext =
  createAuthTest reqMethod (reqUrlT project3.uuid) [reqCtHeader] (reqBodyT project1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    project1ShareEdited
    "Administrate Project"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    requestContext
    project2
    project2ShareEdited
    "Administrate Project"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private, Sharing, Anonymous Disabled)"
    requestContext
    project10
    project10EditedShare
    "Administrate Project"

create_test_403 title requestContext project projectEdited reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT [reqNonAdminAuthHeader]
      let reqBody = reqBodyT projectEdited
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertProject project10) requestContext
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
    "/wizard-api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a/share"
    (reqHeadersT [reqAuthHeader])
    (reqBodyT project1)
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
