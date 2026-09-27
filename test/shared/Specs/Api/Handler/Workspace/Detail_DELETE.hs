module Specs.Api.Handler.Workspace.Detail_DELETE (
  detail_DELETE,
) where

import Data.Aeson (encode)
import Data.Either (isLeft)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.Document.Data.Documents
import qualified Shared.Database.Migration.Development.Document.DocumentMigration as DOC
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KME
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.User.Data.UserGroups
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.User.Role
import Shared.Model.User.UserGroup
import Shared.Model.Workspace.Workspace
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /api/workspaces/{uuid}
-- ------------------------------------------------------------------------
detail_DELETE :: RequestContext -> SpecWith ((), Application)
detail_DELETE requestContext =
  describe "DELETE /api/workspaces/{uuid}" $ do
    test_204 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_404 requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/api/workspaces/3f2b1c0d-9e8f-4a7b-b6c5-d4e3f2a1b0c9"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 requestContext =
  it "HTTP 204 NO CONTENT - the workspace and its projects, documents, editors and user groups are gone" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
    -- AND: Run migrations
    runInContextIO U.runMigration requestContext
    runInContextIO TML.runMigration requestContext
    runInContextIO PRJ.runMigration requestContext
    runInContextIO DOC.runMigration requestContext
    runInContextIO KME.runMigration requestContext
    runInContextIO (insertPackage amsterdamKmPackage) requestContext
    insertSecondWorkspace requestContext
    runInContextIO (insertProject project16) requestContext
    runInContextIO (insertDocument secondWorkspaceDoc) requestContext
    runInContextIO (insertKnowledgeModelEditor secondWorkspaceKnowledgeModelEditor) requestContext
    runInContextIO (insertUserGroup secondWorkspaceGroup) requestContext
    assertCountInDB findProjects requestContext 4
    assertCountInDB findDocuments requestContext 4
    assertCountInDB findKnowledgeModelEditors requestContext 2
    assertCountInDB (findRolesOfWorkspace secondWorkspace.uuid) requestContext 2
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findWorkspaces requestContext 1
    assertCountInDB findProjects requestContext 3
    assertCountInDB findDocuments requestContext 3
    assertCountInDB findKnowledgeModelEditors requestContext 1
    eGroup <- runInContextIO (findUserGroupByUuid secondWorkspaceGroup.uuid) requestContext
    liftIO $ isLeft eGroup `shouldBe` True
    assertCountInDB (findRolesOfWorkspace secondWorkspace.uuid) requestContext 0

findRolesOfWorkspace :: U.UUID -> RequestContextM [Role]
findRolesOfWorkspace workspaceUuid = filter (\role -> role.workspaceUuid == Just workspaceUuid) <$> findRoles

secondWorkspaceGroup :: UserGroup
secondWorkspaceGroup =
  bioGroup
    { uuid = u' "c4d5e6f7-8a9b-4c0d-9e1f-2a3b4c5d6e7f"
    , name = "Second Workspace Group"
    , workspaceUuid = secondWorkspace.uuid
    }

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 requestContext =
  it "HTTP 400 BAD REQUEST - the last workspace can not be deleted" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_SERVICE_WORKSPACE__LAST_WORKSPACE
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod "/api/workspaces/7a1c8e2f-3b4d-4c5e-9f60-1a2b3c4d5e6f" reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findWorkspaces requestContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 requestContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "workspaces.manage"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 requestContext =
  createNotFoundTest'
    reqMethod
    "/api/workspaces/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "workspace"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
