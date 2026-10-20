module Specs.Service.Mcp.Tool.UserGroupTools.SaveUserGroup (
  save_user_group,
) where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as User
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.User.UserGroup
import Shared.Model.User.UserGroupMembership
import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import WizardServer.Model.Context.RequestContext

import Specs.Api.Handler.Common
import Specs.Api.Handler.Mcp.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common
import Specs.Service.Mcp.Tool.UserGroupTools.Common

-- ------------------------------------------------------------------------
-- POST /api/mcp (save_user_group)
-- ------------------------------------------------------------------------
save_user_group :: RequestContext -> SpecWith ((), Application)
save_user_group requestContext =
  describe "POST /api/mcp (save_user_group)" $ do
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
    [ "uuid" .= newGroupUuid
    , "name" .= ("Chemistry Group" :: String)
    , "private" .= False
    , "members" .= [object ["userUuid" .= userNikola.uuid, "mType" .= OwnerUserGroupMembershipType]]
    ]

reqBody = toolCallBody "save_user_group" reqArguments

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
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let Just structuredContent = structuredContentOf response
      liftIO $ valueField "uuid" structuredContent `shouldBe` Just (toJSON newGroupUuid)
      assertToolOutputSchema reqHeaders "save_user_group" structuredContent
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserGroupInDB requestContext newGroup
      assertUserGroupMembershipsInDB requestContext newGroupUuid [newGroupMembership userNikola.uuid OwnerUserGroupMembershipType]
  it "HTTP 200 OK if the call is repeated (the group created once)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      firstResponse <- request reqMethod reqUrl reqHeaders reqBody
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ (valueField "updatedAt" =<< structuredContentOf response) `shouldBe` (valueField "updatedAt" =<< structuredContentOf firstResponse)
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserGroupInDB requestContext newGroup
      assertUserGroupMembershipsInDB requestContext newGroupUuid [newGroupMembership userNikola.uuid OwnerUserGroupMembershipType]
  it "HTTP 200 OK if the change is partial (the other fields and the members kept)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_user_group" (object ["uuid" .= bioGroup.uuid, "name" .= ("Renamed Group" :: String)])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      liftIO $ toolErrorOf response `shouldBe` Nothing
      -- AND: Compare state in DB with expectation
      assertExistenceOfUserGroupInDB requestContext (bioGroup {name = "Renamed Group"} :: UserGroup)
      assertUserGroupMembershipsInDB requestContext bioGroup.uuid [userAlbertBioGroupMembership, userNikolaBioGroupMembership]
  it "HTTP 200 OK if a new group has no workspace in a multi-workspace tenant (tool error)" $
    -- GIVEN: Prepare request
    do
      let reqBody = toolCallBody "save_user_group" (object ["name" .= ("Chemistry Group" :: String), "private" .= False])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toolErrorResult (localeMessage _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED)
      -- AND: Run migrations
      runInContextIO User.runMigration requestContext
      insertSecondWorkspace requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEqualsJson expDto}
      response `shouldRespondWith` responseMatcher
  it "HTTP 200 OK if the user has no userGroups.create and no userGroups.manage (the write tools hidden)" $
    -- GIVEN: Prepare request
    do
      let reqBody = rpcBody "tools/list" (object [])
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Run migrations
      removeAlbertPermissions requestContext [_USER_GROUPS_CREATE_ROLE_PERMISSION, _USER_GROUPS_MANAGE_ROLE_PERMISSION]
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
      response `shouldRespondWith` responseMatcher
      let toolNames = mapMaybe (valueField "name") (toolsOf response)
      liftIO $ filter (`elem` fmap String ["list_user_groups", "save_user_group", "delete_user_group"]) toolNames `shouldBe` [String "list_user_groups"]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody _MCP_READ_ROLE_PERMISSION
