module Specs.Service.Mcp.Tool.ProjectFileTools.DownloadProjectFile (
  download_project_file,
) where

import Control.Monad (void)
import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Tenant
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Public
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Model.User.RolePermission
import Shared.S3.Project.ProjectFileS3
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (download_project_file)
-- ------------------------------------------------------------------------
download_project_file :: RequestContext -> SpecWith ((), Application)
download_project_file requestContext =
  describe "POST /api/mcp (download_project_file)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["file_uuid" .= projectFile.uuid]

reqBody = toolCallBody "download_project_file" reqArguments

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
      runProjectMigrations requestContext
      runInContextIO (insertProjectFile projectFile) requestContext
      runInContextIO (void $ putFile project1.uuid projectFile.uuid projectFile.contentType "hello") requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "contentType" structuredContent `shouldBe` toJSON projectFile.contentType
      liftIO $ jsonField "url" structuredContent `shouldNotBe` Null
      liftIO $ jsonField "expiresAt" structuredContent `shouldNotBe` Null
      assertToolOutputSchema reqHeaders "download_project_file" structuredContent
  it "HTTP 200 OK if the file belongs to another project (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "download_project_file" (object ["project_uuid" .= project2.uuid, "file_uuid" .= projectFile.uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expParams = [("tenant_uuid", U.toString defaultTenantUuid), ("project_uuid", U.toString project2.uuid), ("uuid", U.toString projectFile.uuid)]
      let expDto = toolErrorResult (localeMessage $ _ERROR_DATABASE__ENTITY_NOT_FOUND "project_file" expParams)
      -- AND: Run migrations
      runProjectMigrations requestContext
      runInContextIO (insertProjectFile projectFile) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
