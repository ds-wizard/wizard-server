module Specs.Api.Handler.Workspace.List_GET (
  list_GET,
) where

import Control.Monad (void, when)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as UUID
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Project.ProjectDTO
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Project.Project
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
-- GET /api/workspaces
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/workspaces" $ do
    test_200 requestContext
    test_200_directory requestContext
    test_200_directory_projects requestContext
    test_401 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/workspaces"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK (the only workspace)" requestContext False [reqAuthHeader] (Page "workspaces" (PageMetadata 20 1 1 0) [defaultWorkspace])
  create_test_200 "HTTP 200 OK (only the workspaces the caller is a member of)" requestContext True [reqNonAdminAuthHeader] (Page "workspaces" (PageMetadata 20 1 1 0) [defaultWorkspace])
  create_test_200 "HTTP 200 OK (both workspaces of a member)" requestContext True [reqAuthHeader] (Page "workspaces" (PageMetadata 20 2 1 0) [defaultWorkspace, secondWorkspace])
  create_test_200_reach "HTTP 200 OK (a non-member reaches every workspace through the organization role)" requestContext (Page "workspaces" (PageMetadata 20 2 1 0) [defaultWorkspace, secondWorkspace])

create_test_200 title requestContext withSecondWorkspace reqHeaders expDto =
  it title $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    demoteToResearcher requestContext userNikola
    when withSecondWorkspace (insertSecondWorkspace requestContext)
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?sort=name,asc") reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

create_test_200_reach title requestContext expDto =
  it title $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    insertSecondWorkspace requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?sort=name,asc") [reqNonAdminAuthHeader] reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_200_directory requestContext =
  it "HTTP 200 OK (workspaces.manage lists every workspace, member or not)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (Page "workspaces" (PageMetadata 20 2 1 0) [defaultWorkspace, secondWorkspace])
    -- AND: Run migrations
    insertSecondWorkspace requestContext
    demoteToWorkspaceDirectory requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?sort=name,asc") reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

test_200_directory_projects requestContext =
  it "HTTP 200 OK (workspaces.manage does not open the projects of a workspace)" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode (Page "projects" (PageMetadata 20 0 0 0) ([] :: [ProjectDTO]))
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO TML.runMigration requestContext
    runInContextIO PRJ.runMigration requestContext
    runInContextIO (insertPackage amsterdamKmPackage) requestContext
    insertSecondWorkspace requestContext
    runInContextIO (insertProject (project16 {visibility = VisibleViewProjectVisibility} :: Project)) requestContext
    demoteToWorkspaceDirectory requestContext
    -- WHEN: Call API
    response <- request reqMethod (BS.pack $ "/api/projects?w=" ++ UUID.toString secondWorkspace.uuid) reqHeaders reqBody
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

demoteToWorkspaceDirectory requestContext = do
  runInContextIO (insertRole workspaceDirectoryRole) requestContext
  runInContextIO (updateUserByUuid (userAlbert {role = toRoleSimple workspaceDirectoryRole} :: User)) requestContext
  void $ runInContextIO (deleteWorkspaceMembership secondWorkspace.uuid userAlbert.uuid) requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody
