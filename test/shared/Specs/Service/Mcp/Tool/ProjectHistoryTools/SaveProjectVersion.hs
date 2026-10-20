module Specs.Service.Mcp.Tool.ProjectHistoryTools.SaveProjectVersion (
  save_project_version,
) where

import Data.Aeson
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.DAO.Project.ProjectVersionDAO
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.ProjectVersions
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Event.ProjectEvent
import Shared.Model.Project.Project
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.User.RolePermission
import Shared.Util.Uuid
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Service.Mcp.Tool.ProjectTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_project_version)
-- ------------------------------------------------------------------------
save_project_version :: RequestContext -> SpecWith ((), Application)
save_project_version requestContext =
  describe "POST /api/mcp (save_project_version)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = projectArguments ["uuid" .= newVersionUuid, "name" .= "Final", "event_uuid" .= (sre_rQ10 project1.uuid).uuid]

reqBody = toolCallBody "save_project_version" reqArguments

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
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ jsonField "eventUuid" structuredContent `shouldBe` toJSON (sre_rQ10 project1.uuid).uuid
      assertToolOutputSchema reqHeaders "save_project_version" structuredContent
      -- AND: Compare state in DB with expectation
      versions <- getOneFromDB (findProjectVersionsByProjectUuid project1.uuid) requestContext
      liftIO $ fmap (\version -> (version.uuid, version.name)) versions `shouldMatchList` [((projectVersion1 project1.uuid).uuid, "Version 1"), (newVersionUuid, "Final")]
  it "HTTP 200 OK (a version renamed)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_project_version" (projectArguments ["uuid" .= (projectVersion1 project1.uuid).uuid, "name" .= "Renamed"])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      version <- getOneFromDB (findProjectVersionByUuid (projectVersion1 project1.uuid).uuid) requestContext
      liftIO $ (version.name, version.eventUuid) `shouldBe` ("Renamed", (projectVersion1 project1.uuid).eventUuid)
  it "HTTP 200 OK if event_uuid is left out and the latest event has a version (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_project_version" (projectArguments ["name" .= "Final"])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_PROJECT_VERSION__VERSION_UNIQUENESS (U.toString (slble_rQ1 project1.uuid).uuid))
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createIsaacToolErrorTest "the user only edits the project" requestContext (runProjectMigrations requestContext) editorPermissions reqBody (_ERROR_VALIDATION__FORBIDDEN "Administrate Project")
  it "HTTP 200 OK if the uuid belongs to a version of another project (tool error)" $
    -- GIVEN: Prepare request
    do
      let takenUuid = (projectVersion1 project2Uuid).uuid
      let reqBody = toolCallBody "save_project_version" (projectArguments ["uuid" .= takenUuid, "name" .= "Final", "event_uuid" .= (sre_rQ10 project1.uuid).uuid])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage $ _ERROR_SERVICE_PROJECT_VERSION__UUID_TAKEN (U.toString takenUuid))
      -- AND: Run migrations
      runProjectMigrations requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  createToolNoPermissionTest requestContext reqBody _MCP_WRITE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION

newVersionUuid :: U.UUID
newVersionUuid = u' "6a1c3e5f-7b9d-4f2a-8c4e-1d3f5a7b9c02"
