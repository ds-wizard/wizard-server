module Specs.Api.Handler.UserGroup.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.User.Group.UserGroupDetailJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.User.UserGroupMembership
import Shared.Service.User.Group.UserGroupMapper
import Shared.Service.User.WizardUserMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/user-groups/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/user-groups/{uuid}" $ do
    test_200 requestContext
    test_200_manage requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_single_workspace requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/user-groups/6ab8ecc3-c0f5-4864-a055-b0096ca55569"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = toDetailDTO bioGroup [toWithMembership userNikola OwnerUserGroupMembershipType, toWithMembership userAlbert OwnerUserGroupMembershipType]
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_200_manage requestContext =
  it "HTTP 200 OK (userGroups.manage on the workspace role opens a private group)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = toDetailDTO bioGroup [toWithMembership userNikola OwnerUserGroupMembershipType, toWithMembership userAlbert OwnerUserGroupMembershipType]
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userIsaac.uuid defaultWorkspaceAdminRole.uuid) requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqIsaacAuthTokenHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext =
  it "HTTP 403 FORBIDDEN (private group, no membership, no userGroups.manage)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View UserGroup")
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqIsaacAuthTokenHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_403_single_workspace requestContext =
  it "HTTP 403 FORBIDDEN (a single-workspace tenant ignores userGroups.manage on the workspace role)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View UserGroup")
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userIsaac.uuid defaultWorkspaceAdminRole.uuid) requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl [reqIsaacAuthTokenHeader] reqBody
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
    "/api/user-groups/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "user_group"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
