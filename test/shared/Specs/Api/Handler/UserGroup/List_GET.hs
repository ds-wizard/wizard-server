module Specs.Api.Handler.UserGroup.List_GET (
  list_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.User.Group.UserGroupListJM ()
import Shared.Database.Migration.Development.User.Data.UserGroups
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Model.User.UserGroupList
import Shared.Service.User.Group.UserGroupMapper
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.UserGroup.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/user-groups
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/user-groups" $ do
    test_200 requestContext
    test_200_workspace requestContext
    test_200_unreachable_workspace requestContext
    test_400_tenant requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/user-groups?sort=uuid,asc"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (Page "userGroups" (PageMetadata 20 2 1 0) [toList bioGroup True True, toList plantGroup True False]) response

test_200_workspace requestContext =
  it "HTTP 200 OK (w narrows to one workspace)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    enableMultiWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "&q=plant&" <> workspaceQuery) reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (Page "userGroups" (PageMetadata 20 1 1 0) [toList plantGroup True False]) response

test_200_unreachable_workspace requestContext =
  it "HTTP 200 OK (w the user does not reach gives an empty page)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "&" <> secondWorkspaceQuery) [reqIsaacAuthTokenHeader] reqBody
    -- THEN: Compare response with expectation
    expectJson 200 (Page "userGroups" (PageMetadata 20 0 0 0) ([] :: [UserGroupList])) response

test_400_tenant requestContext =
  it "HTTP 400 BAD REQUEST (tenant=true)" $ do
    -- GIVEN: Run migrations
    runInContextIO U.runMigration requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "&tenant=true") reqHeaders reqBody
    -- THEN: Compare response with expectation
    expectJson 400 (UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED) response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
