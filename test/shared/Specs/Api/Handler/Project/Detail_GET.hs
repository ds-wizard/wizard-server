module Specs.Api.Handler.Project.Detail_GET (
  detail_GET,
) where

import Control.Monad (unless, void, when)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailDTO
import Shared.Constant.Workspace
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.WizardKnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.User.Public
import Shared.Model.Error.Error
import Shared.Model.Project.Project
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper (toSuggestion)
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/projects/{projectUuid}
-- ------------------------------------------------------------------------
detail_GET :: RequestContext -> SpecWith ((), Application)
detail_GET requestContext =
  describe "GET /api/projects/{projectUuid}" $ do
    test_200 requestContext
    test_200_workspace_role requestContext
    test_200_member_visible requestContext
    test_403 requestContext
    test_403_workspace_role requestContext
    test_403_non_member_visible requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT projectUuid = BS.pack $ "/api/projects/" ++ U.toString projectUuid

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    requestContext
    project1
    project1Events
    germanyPackageSuggestion
    [reqAuthHeader]
    [project1AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleView)"
    requestContext
    project2
    project2Events
    germanyPackageSuggestion
    [reqNonAdminAuthHeader]
    [project2AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Commenter)"
    requestContext
    (project13 {visibility = PrivateProjectVisibility})
    project13Events
    germanyPackageSuggestion
    [reqNonAdminAuthHeader]
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Commenter, VisibleComment)"
    requestContext
    project13
    project13Events
    germanyPackageSuggestion
    [reqIsaacAuthTokenHeader]
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    requestContext
    (project13 {sharing = AnyoneWithLinkCommentProjectSharing})
    project13Events
    germanyPackageSuggestion
    []
    [project13NikolaCommentProjectPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    requestContext
    project7
    project7Events
    germanyPackageSuggestion
    []
    [project7AlbertEditProjectPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    requestContext
    project3
    project3Events
    germanyPackageSuggestion
    [reqNonAdminAuthHeader]
    []
  create_test_200
    "HTTP 200 OK (Anonymous, Public, Sharing)"
    requestContext
    project10
    project10Events
    germanyPackageSuggestion
    []
    []

test_200_workspace_role requestContext = do
  create_test_200_workspace_role
    "HTTP 200 OK (projects.view on the workspace role of the project's workspace)"
    requestContext
    secondWorkspaceAdminRole
    True
  create_test_200_workspace_role
    "HTTP 200 OK (projects.view on the organization role reaches every workspace)"
    requestContext
    secondWorkspaceUserRole
    False

create_test_200_workspace_role title requestContext secondWorkspaceRole organizationRoleWithoutView =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            ProjectDetailDTO
              { uuid = project16.uuid
              , name = project16.name
              , sharing = project16.sharing
              , visibility = project16.visibility
              , knowledgeModelPackage = toSuggestion amsterdamKmPackage
              , isTemplate = project16.isTemplate
              , permissions = [project16NikolaEditProjectPermDto]
              , fileCount = 0
              , workspaceUuid = project16.workspaceUuid
              }
      let expBody = encode expDto
      -- AND: Run migrations
      prepareSecondWorkspaceProject requestContext secondWorkspaceRole organizationRoleWithoutView
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project16.uuid) (reqHeadersT [reqAuthHeader]) reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_403_workspace_role requestContext =
  it "HTTP 403 FORBIDDEN (projects.view on the role of another workspace)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"
      let expBody = encode expDto
      -- AND: Run migrations
      prepareSecondWorkspaceProject requestContext secondWorkspaceUserRole True
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project16.uuid) (reqHeadersT [reqAuthHeader]) reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

prepareSecondWorkspaceProject requestContext secondWorkspaceRole organizationRoleWithoutView = do
  runInContextIO U.runMigration requestContext
  runInContextIO TML.runMigration requestContext
  runInContextIO PRJ.runMigration requestContext
  runInContextIO (insertPackage amsterdamKmPackage) requestContext
  insertSecondWorkspace requestContext
  runInContextIO (insertProject project16) requestContext
  let userWithoutView = userWithoutPerm requestContext.serverConfig _PROJECTS_VIEW_ROLE_PERMISSION
  when organizationRoleWithoutView $
    void (runInContextIO (updateUserByUuid userWithoutView) requestContext)
  runInContextIO (updateWorkspaceMembershipRole secondWorkspace.uuid userAlbert.uuid secondWorkspaceRole.uuid) requestContext
  void $ runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceAdminRole.uuid) requestContext

test_200_member_visible requestContext = do
  create_test_visible
    "HTTP 200 OK (member without projects.view, VisibleView)"
    requestContext
    (project2 {permissions = []} :: Project)
    project2Events
    True
  create_test_visible
    "HTTP 200 OK (member without projects.view, VisibleEdit)"
    requestContext
    project3
    project3Events
    True

test_403_non_member_visible requestContext = do
  create_test_visible
    "HTTP 403 FORBIDDEN (non-member without reach, VisibleView)"
    requestContext
    (project2 {permissions = []} :: Project)
    project2Events
    False
  create_test_visible
    "HTTP 403 FORBIDDEN (non-member without reach, VisibleEdit)"
    requestContext
    project3
    project3Events
    False

create_test_visible title requestContext project projectEvents isMember =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expHeaders = resCtHeader : resCorsHeaders
      let expDetail =
            ProjectDetailDTO
              { uuid = project.uuid
              , name = project.name
              , sharing = project.sharing
              , visibility = project.visibility
              , knowledgeModelPackage = germanyPackageSuggestion
              , isTemplate = project.isTemplate
              , permissions = []
              , fileCount = 0
              , workspaceUuid = project.workspaceUuid
              }
      let (expStatus, expBody) =
            if isMember
              then (200, encode expDetail)
              else (403, encode (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Project"))
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject project) requestContext
      runInContextIO (insertProjectEvents projectEvents) requestContext
      demoteToResearcher requestContext userAlbert
      unless isMember $
        void (runInContextIO (deleteWorkspaceMembership defaultWorkspaceUuid userAlbert.uuid) requestContext)
      -- WHEN: Call API
      response <- request reqMethod (reqUrlT project.uuid) (reqHeadersT [reqAuthHeader]) reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

create_test_200 title requestContext project projectEvents kmPackage authHeader permissions =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO (insertProject project) requestContext
      runInContextIO (insertProjectEvents projectEvents) requestContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            ProjectDetailDTO
              { uuid = project.uuid
              , name = project.name
              , sharing = project.sharing
              , visibility = project.visibility
              , knowledgeModelPackage = kmPackage
              , isTemplate = project.isTemplate
              , permissions = permissions
              , fileCount = 0
              , workspaceUuid = project.workspaceUuid
              }
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
test_403 requestContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    requestContext
    project1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Project")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    requestContext
    project2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    requestContext
    project3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title requestContext project authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT project.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
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
    "/api/projects/f08ead5f-746d-411b-aee6-77ea3d24016a"
    [reqHeadersT reqAuthHeader]
    reqBody
    "project"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
