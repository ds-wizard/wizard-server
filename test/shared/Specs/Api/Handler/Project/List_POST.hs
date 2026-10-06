module Specs.Api.Handler.Project.List_POST (
  list_POST,
) where

import Control.Monad (when)
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
import Shared.Api.Resource.Project.ProjectCreateDTO
import Shared.Api.Resource.Project.ProjectCreateJM ()
import Shared.Api.Resource.Project.ProjectDTO
import Shared.Constant.Tenant
import Shared.Constant.Workspace
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectEventDAO
import Shared.Database.DAO.Settings.SettingsProjectsDAO
import Shared.Database.DAO.User.UserDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Project.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /api/projects
-- ------------------------------------------------------------------------
list_POST :: RequestContext -> SpecWith ((), Application)
list_POST requestContext =
  describe "POST /api/projects" $ do
    test_201 requestContext
    test_201_workspace requestContext
    test_201_workspace_override requestContext
    test_400 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/api/projects"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT project = project

reqBodyT project = encode (reqDtoT project)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 requestContext = do
  create_test_201 requestContext "HTTP 201 CREATED (with token)" False project1Create [reqAuthHeader]
  create_test_201
    requestContext
    "HTTP 201 CREATED (without token)"
    True
    (project1Create {sharing = AnyoneWithLinkEditProjectSharing} :: ProjectCreateDTO)
    []

create_test_201 requestContext title anonymousSharingEnabled project authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT project
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto =
            if anonymousSharingEnabled
              then project1Dto {sharing = AnyoneWithLinkEditProjectSharing} :: ProjectDTO
              else project1Dto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO deleteProjects requestContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing requestContext anonymousSharingEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareProjectCreateDtos resBody expDto
      -- AND: Find a result in DB
      (Right eventsInDB) <- runInContextIO (findProjectEventsByProjectUuid resBody.uuid) requestContext
      if anonymousSharingEnabled
        then
          assertExistenceOfProjectInDB
            requestContext
            ( project1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , sharing = AnyoneWithLinkEditProjectSharing
                , projectTags = []
                , permissions = []
                , creatorUuid = Nothing
                }
                :: Project
            )
            eventsInDB
        else do
          let aPermissions =
                [ (head project1.permissions)
                    { projectUuid = resBody.uuid
                    }
                    :: ProjectPerm
                ]
          assertExistenceOfProjectInDB
            requestContext
            ( project1
                { uuid = resBody.uuid
                , description = Nothing
                , isTemplate = False
                , projectTags = []
                , permissions = aPermissions
                }
                :: Project
            )
            eventsInDB

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext = do
  createInvalidJsonTest reqMethod reqUrl "packageId"
  create_test_400 "HTTP 400 BAD REQUEST - w in a single-workspace tenant" requestContext False (reqUrl <> "?w=7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f") _ERROR_SERVICE_WORKSPACE__WORKSPACE_NOT_ACCEPTED
  create_test_400 "HTTP 400 BAD REQUEST - missing w in a multi-workspace tenant" requestContext True reqUrl _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED
  create_test_400 "HTTP 400 BAD REQUEST - tenant=true on a workspace-only entity" requestContext False (reqUrl <> "?tenant=true") _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED

create_test_400 title requestContext multiWorkspace reqUrl expError =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project1Create
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError expError)
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      when multiWorkspace (enableMultiWorkspace requestContext)
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

test_201_workspace requestContext = do
  create_test_201_workspace "HTTP 201 CREATED (with token, multi-workspace tenant with w)" requestContext False [reqAuthHeader]
  create_test_201_workspace "HTTP 201 CREATED (without token, multi-workspace tenant with w)" requestContext True []

create_test_201_workspace title requestContext anonymousSharingEnabled authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqDto =
            if anonymousSharingEnabled
              then project1Create {sharing = AnyoneWithLinkEditProjectSharing} :: ProjectCreateDTO
              else project1Create
      let reqBody = reqBodyT reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expCreatorUuid = if anonymousSharingEnabled then Nothing else Just userAlbert.uuid
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
      runInContextIO deleteProjects requestContext
      enableMultiWorkspace requestContext
      insertSecondWorkspace requestContext
      -- AND: Enabled anonymous sharing
      updateAnonymousProjectSharing requestContext anonymousSharingEnabled
      -- WHEN: Call API
      response <- request reqMethod (reqUrl <> "?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9") reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.workspaceUuid `shouldBe` secondWorkspace.uuid
      -- AND: Find a result in DB
      projectInDB <- getOneFromDB (findProjectByUuid resBody.uuid) requestContext
      liftIO $ projectInDB.workspaceUuid `shouldBe` secondWorkspace.uuid
      liftIO $ projectInDB.creatorUuid `shouldBe` expCreatorUuid

test_201_workspace_override requestContext =
  it "HTTP 201 CREATED (the projects override of the workspace sets the visibility)" $ do
    -- GIVEN: Prepare request
    let reqHeaders = reqHeadersT [reqAuthHeader]
    let reqBody = reqBodyT project1Create
    -- AND: Prepare expectation
    let visibility = SettingsProjectsVisibility {enabled = False, defaultValue = VisibleViewProjectVisibility}
    let override = settingsProjects {projectVisibility = visibility} :: SettingsProjects
    -- AND: Run migrations
    runInContextIO TML.runMigration requestContext
    runInContextIO (insertPackage germanyKmPackage) requestContext
    runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) requestContext
    runInContextIO deleteProjects requestContext
    insertSecondWorkspace requestContext
    runInContextIO (saveSettingsProjects defaultTenantUuid (Just secondWorkspaceUuid) override) requestContext
    -- WHEN: Call API
    response <- request reqMethod (reqUrl <> "?w=3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9") reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, _, resBody) = destructResponse response :: (Int, ResponseHeaders, ProjectDTO)
    assertResStatus status 201
    liftIO $ resBody.visibility `shouldBe` VisibleViewProjectVisibility
    -- AND: Find a result in DB
    projectInDB <- getOneFromDB (findProjectByUuid resBody.uuid) requestContext
    liftIO $ projectInDB.visibility `shouldBe` VisibleViewProjectVisibility

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext = do
  create_test_404
    "HTTP 404 NOT FOUND - workspace of another tenant"
    requestContext
    insertSecondWorkspace
    (reqUrl <> "?w=" <> BS.pack (U.toString differentWorkspaceUuid))
    project1Create
    "workspace"
  create_test_404
    "HTTP 404 NOT FOUND - not a member of w"
    requestContext
    insertSecondWorkspaceWithoutCaller
    (reqUrl <> "?w=" <> BS.pack (U.toString secondWorkspaceUuid))
    project1Create
    "workspace"
  create_test_404
    "HTTP 404 NOT FOUND - package of another workspace"
    requestContext
    insertSecondWorkspace
    (reqUrl <> "?w=" <> BS.pack (U.toString defaultWorkspaceUuid))
    (project1Create {knowledgeModelPackageUuid = secondWorkspaceKmPackage.uuid} :: ProjectCreateDTO)
    "knowledge_model_package"

test_403 requestContext =
  it "HTTP 403 FORBIDDEN (no projects.create)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT project1Create
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN ("Missing permission: " ++ _PROJECTS_CREATE_ROLE_PERMISSION))
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      runInContextIO (updateUserByUuid (userWithoutPerm requestContext.serverConfig _PROJECTS_CREATE_ROLE_PERMISSION)) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

create_test_404 title requestContext prepareWorkspace reqUrl reqDto entityName =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT reqDto
      -- AND: Prepare expectation
      let expStatus = 404
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (NotExistsError (_ERROR_VALIDATION__ABSENCE entityName))
      -- AND: Run migrations
      runInContextIO TML.runMigration requestContext
      prepareWorkspace requestContext
      runInContextIO (insertPackage germanyKmPackage) requestContext
      runInContextIO (insertPackage secondWorkspaceKmPackage) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

secondWorkspaceKmPackage :: KnowledgeModelPackage
secondWorkspaceKmPackage =
  germanyKmPackage
    { uuid = u' "4a3b2c1d-0e9f-4a8b-9c7d-6e5f4a3b2c1d"
    , workspaceUuid = Just secondWorkspace.uuid
    }
