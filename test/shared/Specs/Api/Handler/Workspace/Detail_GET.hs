module Specs.Api.Handler.Workspace.Detail_GET (
  detail_GET,
) where

import Control.Monad (void)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Service.User.RoleMapper (toRoleSimple)
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/workspaces/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/workspaces/{uuid}" $ do
    test_200 requestContext
    test_200_reach requestContext
    test_200_directory requestContext
    test_401 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT workspace = BS.pack $ "/api/workspaces/" ++ U.toString workspace.uuid

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
    let expBody = encode defaultWorkspace
    -- WHEN: Call API
    response <- request reqMethod (reqUrlT defaultWorkspace) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_reach requestContext =
  it "HTTP 200 OK (a non-member reaches the workspace through the organization role)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode secondWorkspace
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrlT secondWorkspace) [reqNonAdminAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_200_directory requestContext =
  it "HTTP 200 OK (a non-member opens the workspace through workspaces.manage)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode secondWorkspace
    -- AND: Run migrations
    insertSecondWorkspace requestContext
    runInContextIO (insertRole workspaceDirectoryRole) requestContext
    runInContextIO (updateUserByUuid (userAlbert {role = toRoleSimple workspaceDirectoryRole} :: User)) requestContext
    void $ runInContextIO (deleteWorkspaceMembership secondWorkspace.uuid userAlbert.uuid) requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrlT secondWorkspace) reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

workspaceDirectoryRole :: Role
workspaceDirectoryRole =
  researcherRole
    { uuid = u' "a0000000-0000-0000-0000-0000000000fe"
    , name = "Workspace Directory"
    , permissions = [_WORKSPACES_MANAGE_ROLE_PERMISSION]
    }

test_401 requestContext = createAuthTest reqMethod (reqUrlT defaultWorkspace) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  it "HTTP 404 NOT FOUND - the caller is not a member" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 404
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    demoteToResearcher requestContext userNikola
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrlT secondWorkspace) [reqNonAdminAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
  it "HTTP 404 NOT FOUND - the workspace does not exist" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 404
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = NotExistsError (_ERROR_VALIDATION__ABSENCE "workspace")
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod "/api/workspaces/deab6c38-aeac-4b17-a501-4365a0a70176" reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
