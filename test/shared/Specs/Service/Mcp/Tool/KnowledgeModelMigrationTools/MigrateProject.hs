module Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.MigrateProject (
  migrate_project,
) where

import Control.Monad
import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.User.UserDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.NikolaTesla
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import Shared.Model.User.RoleSimple
import Shared.Model.User.User
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common
import Specs.Service.Mcp.Tool.KnowledgeModelMigrationTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (migrate_project)
-- ------------------------------------------------------------------------
migrate_project :: RequestContext -> SpecWith ((), Application)
migrate_project requestContext =
  describe "POST /api/mcp (migrate_project)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["project_uuid" .= project4.uuid, "target_package_uuid" .= netherlandsKmPackageV2.uuid]

reqBody = toolCallBody "migrate_project" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertProject project4) requestContext
      runInContextIO (insertProjectEvents project4Events) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ (valueField "uuid" =<< valueField "knowledgeModelPackage" structuredContent) `shouldBe` Just (toJSON netherlandsKmPackageV2.uuid)
      assertToolOutputSchema reqHeaders "migrate_project" structuredContent
      -- AND: Compare state in DB with expectation
      assertProjectPackageInDB requestContext project4 netherlandsKmPackageV2.uuid
  it "HTTP 200 OK if the user may not migrate the project (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = [reqNonAdminAuthHeader, reqCtHeader]
      let reqBody = toolCallBody "migrate_project" (object ["project_uuid" .= project1.uuid, "target_package_uuid" .= netherlandsKmPackageV2.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage (_ERROR_VALIDATION__FORBIDDEN "Migrate Project"))
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      let permissions = userNikola.role.permissions ++ [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION]
      void $ runInContextIO (updateUserByUuid (userNikola {role = userNikola.role {permissions = permissions} :: RoleSimple} :: User)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertProjectPackageInDB requestContext project1 project1.knowledgeModelPackageUuid

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
