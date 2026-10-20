module Specs.Service.Mcp.Tool.WorkspaceTools.SetWorkspaceMembers (
  set_workspace_members,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Constant.Workspace
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.WizardPublic
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common
import Specs.Service.Mcp.Tool.WorkspaceTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (set_workspace_members)
-- ------------------------------------------------------------------------
set_workspace_members :: RequestContext -> SpecWith ((), Application)
set_workspace_members requestContext =
  describe "POST /api/mcp (set_workspace_members)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments =
  object
    [ "workspace_uuid" .= secondWorkspaceUuid
    , "members" .= [object ["userUuid" .= userNikola.uuid], object ["userUuid" .= userAlbert.uuid, "roleUuid" .= secondWorkspaceAdminRole.uuid]]
    ]

reqBody = toolCallBody "set_workspace_members" reqArguments

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
      runInContextIO User.runMigration requestContext
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      assertToolOutputSchema reqHeaders "set_workspace_members" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfWorkspaceMembershipInDB requestContext secondWorkspaceUuid userNikola.uuid (Just secondWorkspaceUserRole.uuid)
      assertExistenceOfWorkspaceMembershipInDB requestContext secondWorkspaceUuid userAlbert.uuid (Just secondWorkspaceAdminRole.uuid)
  it "HTTP 200 OK if a member is removed" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_workspace_members" (object ["workspace_uuid" .= secondWorkspaceUuid, "remove" .= [userNikola.uuid]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolResult (object ["members" .= ([] :: [Value]), "removed" .= [userNikola.uuid]])
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      insertSecondWorkspace requestContext
      runInContextIO (insertWorkspaceMembership (secondWorkspaceMembership userNikola)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfWorkspaceMembershipInDB requestContext secondWorkspaceUuid userNikola.uuid
  it "HTTP 200 OK if the role belongs to another workspace (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_workspace_members" (object ["workspace_uuid" .= secondWorkspaceUuid, "members" .= [object ["userUuid" .= userNikola.uuid, "roleUuid" .= defaultWorkspaceAdminRole.uuid]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_VALIDATION__USER_ROLE_NOT_IN_WORKSPACE)
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      assertAbsenceOfWorkspaceMembershipInDB requestContext secondWorkspaceUuid userNikola.uuid
  it "HTTP 200 OK if the tenant has a single workspace (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "set_workspace_members" (object ["workspace_uuid" .= defaultWorkspaceUuid, "members" .= [object ["userUuid" .= userNikola.uuid]]])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__WORKSPACE_PLANE_UNAVAILABLE)
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
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
