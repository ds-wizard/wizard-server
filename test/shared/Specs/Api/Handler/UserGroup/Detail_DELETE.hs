module Specs.Api.Handler.UserGroup.Detail_DELETE (
  detail_DELETE,
) where

import Data.Maybe (isJust, isNothing)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectCacheDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectPermDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.UserGroups
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.User.UserGroup
import Shared.Service.Project.Cache.ProjectCacheService
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.UserGroup.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/user-groups/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/user-groups/{uuid}" $ do
    test_204 requestContext
    test_204_projects requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/user-groups/6ab8ecc3-c0f5-4864-a055-b0096ca55569"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response
    -- AND: Find result in DB and compare with expectation state
    memberships <- getOneFromDB (findUserGroupMembershipsByUserGroupUuid bioGroup.uuid) requestContext
    liftIO $ memberships `shouldBe` []
    detailResponse <- request methodGet reqUrl reqHeaders reqBody
    expectStatus 404 detailResponse

test_204_projects requestContext =
  it "HTTP 204 NO CONTENT (drops the group's project permissions and the project caches)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO TML.runMigration requestContext
    runInContextIO PRJ.runMigration requestContext
    runInContextIO (insertProject project15) requestContext
    runInContextIO refreshProjectCaches requestContext
    mCacheBefore <- getOneFromDB (findProjectCacheByProjectUuid' project15.uuid) requestContext
    liftIO $ isJust mCacheBefore `shouldBe` True
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectStatus 204 response
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB (findProjectPermsFiltered [("project_uuid", U.toString project15.uuid)]) requestContext 0
    mCacheAfter <- getOneFromDB (findProjectCacheByProjectUuid' project15.uuid) requestContext
    liftIO $ isNothing mCacheAfter `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN (neither owner nor userGroups.manage)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqIsaacAuthTokenHeader] reqBody
    -- THEN: Compare response with expectation
    expectJson 403 (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit UserGroup") response
