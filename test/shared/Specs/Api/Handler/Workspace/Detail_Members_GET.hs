module Specs.Api.Handler.Workspace.Detail_Members_GET (
  detail_members_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Workspace.WorkspaceMemberJM ()
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Service.Workspace.WorkspaceMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/workspaces/{uuid}/members
-- ------------------------------------------------------------------------
detail_members_GET :: RequestContext -> SpecWith ((), Application)
detail_members_GET requestContext =
  describe "GET /api/workspaces/{uuid}/members" $ do
    test_200 requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f/members"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    (reqUrl <> "?sort=email,asc")
    (Page "members" (PageMetadata 20 1 1 0) [toMember (defaultWorkspaceMembership userAlbert) (toRoleSimple defaultWorkspaceUserRole) userAlbert])
  create_test_200
    "HTTP 200 OK (sort by uuid)"
    requestContext
    (reqUrl <> "?sort=uuid,asc")
    (Page "members" (PageMetadata 20 1 1 0) [toMember (defaultWorkspaceMembership userAlbert) (toRoleSimple defaultWorkspaceUserRole) userAlbert])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    (reqUrl <> "?q=Einstein")
    (Page "members" (PageMetadata 20 1 1 0) [toMember (defaultWorkspaceMembership userAlbert) (toRoleSimple defaultWorkspaceUserRole) userAlbert])

create_test_200 title requestContext reqUrl expDto =
  it title $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
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
test_404 requestContext =
  it "HTTP 404 NOT FOUND - the caller is not a member" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 404
    let expHeaders = resCtHeader : resCorsHeaders
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    demoteToResearcher requestContext userNikola
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod "/api/workspaces/3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9/members" [reqNonAdminAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals (encode (NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")))}
    response `shouldRespondWith` responseMatcher
