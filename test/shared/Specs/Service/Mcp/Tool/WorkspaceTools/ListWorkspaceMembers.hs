module Specs.Service.Mcp.Tool.WorkspaceTools.ListWorkspaceMembers (
  list_workspace_members,
) where

import Data.Aeson
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Workspace.WorkspaceMemberJM ()
import Shared.Constant.Workspace
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.User.RolePermission
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Service.Workspace.WorkspaceMapper (toMember)
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (list_workspace_members)
-- ------------------------------------------------------------------------
list_workspace_members :: RequestContext -> SpecWith ((), Application)
list_workspace_members requestContext =
  describe "POST /api/mcp (list_workspace_members)" $ do
    test_200 requestContext
    test_401 requestContext
    test_403 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/mcp"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqArguments = object ["workspace_uuid" .= defaultWorkspaceUuid]

reqBody = toolCallBody "list_workspace_members" reqArguments

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext reqArguments
  create_test_200 "HTTP 200 OK if a query is given" requestContext (object ["workspace_uuid" .= defaultWorkspaceUuid, "q" .= ("Einstein" :: String)])

create_test_200 title requestContext reqArguments =
  it title $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "list_workspace_members" reqArguments
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expMember = toMember (defaultWorkspaceMembership userAlbert) (toRoleSimple defaultWorkspaceUserRole) userAlbert
      let expContent = toJSON (Page "members" (PageMetadata 20 1 1 0) [expMember])
      let expDto = toolResult expContent
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
      assertToolOutputSchema reqHeaders "list_workspace_members" expContent

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
