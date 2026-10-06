module Specs.Api.Handler.KnowledgeModelEditor.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Common.WizardPageJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/knowledge-model-editors
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/knowledge-model-editors" $ do
    test_200 requestContext
    test_200_workspace requestContext
    test_200_workspace_role requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_not_member requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/knowledge-model-editors"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200 "HTTP 200 OK" requestContext "/api/knowledge-model-editors" (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [amsterdamKnowledgeModelEditorList])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    "/api/knowledge-model-editors?q=Amsterdam Knowledge Model"
    (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [amsterdamKnowledgeModelEditorList])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    requestContext
    "/api/knowledge-model-editors?q=Non-existing KM Editor"
    (Page "knowledgeModelEditors" (PageMetadata 20 0 0 0) [])
  create_test_200_outdated "HTTP 200 OK (outdated editor)" requestContext

create_test_200 title requestContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Page KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto

create_test_200_outdated title requestContext =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [amsterdamKnowledgeModelEditorList {state = OutdatedKnowledgeModelEditorState}]
      -- AND: Run migrations
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      runInContextIO (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) requestContext
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO (insertPackage netherlandsKmPackageV2) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Page KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200_workspace requestContext = do
  create_test_200_workspace
    "HTTP 200 OK (all workspaces of the caller)"
    requestContext
    insertSecondWorkspace
    "/api/knowledge-model-editors?sort=createdAt,asc"
    (Page "knowledgeModelEditors" (PageMetadata 20 2 1 0) [amsterdamKnowledgeModelEditorList, secondWorkspaceKnowledgeModelEditorList])
  create_test_200_workspace
    "HTTP 200 OK (w)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/knowledge-model-editors?w=" ++ U.toString secondWorkspace.uuid)
    (Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [secondWorkspaceKnowledgeModelEditorList])
  create_test_200_workspace
    "HTTP 200 OK (workspace of another tenant)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/knowledge-model-editors?w=" ++ U.toString differentWorkspaceUuid)
    (Page "knowledgeModelEditors" (PageMetadata 20 0 0 0) [])

test_200_workspace_role requestContext =
  it "HTTP 200 OK (workspace role grants the editors of its workspace only)" $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      insertSecondWorkspace requestContext
      runInContextIO (insertKnowledgeModelEditor secondWorkspaceKnowledgeModelEditor) requestContext
      runInContextIO (updateUserByUuid (userWithoutPerm requestContext.serverConfig _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION)) requestContext
      runInContextIO (updateWorkspaceMembershipRole secondWorkspace.uuid userAlbert.uuid secondWorkspaceAdminRole.uuid) requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/knowledge-model-editors" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Page KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` Page "knowledgeModelEditors" (PageMetadata 20 1 1 0) [secondWorkspaceKnowledgeModelEditorList]

create_test_200_workspace title requestContext prepareWorkspace reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      prepareWorkspace requestContext
      runInContextIO (insertKnowledgeModelEditor secondWorkspaceKnowledgeModelEditor) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, Page KnowledgeModelEditorList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST (tenant=true on a workspace-only entity)" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode (UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED)
      -- AND: Run migrations
      runInContextIO (deletePackageByUuid netherlandsKmPackageV2.uuid) requestContext
      runInContextIO KnowledgeModelEditor.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/knowledge-model-editors?tenant=true" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "knowledgeModels.useEditor"

test_403_not_member requestContext =
  createNotMemberTest requestContext (runInContextIO KnowledgeModelEditor.runMigration requestContext) reqMethod (BS.pack $ "/api/knowledge-model-editors?w=" ++ U.toString secondWorkspaceUuid) reqBody "knowledgeModels.useEditor"
