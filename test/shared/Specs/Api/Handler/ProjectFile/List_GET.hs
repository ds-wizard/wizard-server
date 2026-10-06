module Specs.Api.Handler.ProjectFile.List_GET (
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

import Shared.Api.Resource.Common.PageJM ()
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Project.File.ProjectFileListJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.ProjectFiles
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ
import Shared.Database.Migration.Development.Tenant.Data.WizardTenants
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Error.Error
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.File.ProjectFileList
import Shared.Model.Project.Project
import Shared.Model.Project.ProjectSimple
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Util.Uuid
import WizardServer.Model.Context.RequestContext

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /api/project-files
-- ------------------------------------------------------------------------
list_GET :: RequestContext -> SpecWith ((), Application)
list_GET requestContext =
  describe "GET /api/project-files" $ do
    test_200 requestContext
    test_400 requestContext
    test_401 requestContext
    test_403 requestContext
    test_403_not_member requestContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/api/project-files"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 requestContext = do
  create_test_200
    "HTTP 200 OK"
    requestContext
    insertSecondWorkspace
    "/api/project-files?sort=fileName,asc"
    (Page "projectFiles" (PageMetadata 20 2 1 0) [projectFileList, secondWorkspaceProjectFileList])
  create_test_200
    "HTTP 200 OK (query)"
    requestContext
    insertSecondWorkspace
    "/api/project-files?q=second"
    (Page "projectFiles" (PageMetadata 20 1 1 0) [secondWorkspaceProjectFileList])
  create_test_200
    "HTTP 200 OK (w)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/project-files?w=" ++ U.toString secondWorkspace.uuid)
    (Page "projectFiles" (PageMetadata 20 1 1 0) [secondWorkspaceProjectFileList])
  create_test_200
    "HTTP 200 OK (workspace of another tenant)"
    requestContext
    insertSecondWorkspace
    (BS.pack $ "/api/project-files?w=" ++ U.toString differentWorkspaceUuid)
    (Page "projectFiles" (PageMetadata 20 0 0 0) ([] :: [ProjectFileList]))

create_test_200 title requestContext prepareWorkspace reqUrl expDto =
  it title $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      runInContextIO (insertPackage amsterdamKmPackage) requestContext
      prepareWorkspace requestContext
      runInContextIO (insertProject project16) requestContext
      runInContextIO deleteProjectFiles requestContext
      runInContextIO (insertProjectFile projectFile1) requestContext
      runInContextIO (insertProjectFile secondWorkspaceProjectFile) requestContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

projectFile1 :: ProjectFile
projectFile1 =
  ProjectFile
    { uuid = projectFileList.uuid
    , fileName = projectFileList.fileName
    , contentType = projectFileList.contentType
    , fileSize = projectFileList.fileSize
    , projectUuid = project1.uuid
    , createdBy = Just userAlbert.uuid
    , tenantUuid = defaultTenant.uuid
    , createdAt = projectFileList.createdAt
    }

secondWorkspaceProjectFile :: ProjectFile
secondWorkspaceProjectFile =
  projectFile1
    { uuid = u' "1f2e3d4c-5b6a-4798-8a9b-0c1d2e3f4a5b"
    , fileName = "second_workspace_file.txt"
    , projectUuid = project16.uuid
    }

secondWorkspaceProjectFileList :: ProjectFileList
secondWorkspaceProjectFileList =
  projectFileList
    { uuid = secondWorkspaceProjectFile.uuid
    , fileName = secondWorkspaceProjectFile.fileName
    , project = ProjectSimple {uuid = project16.uuid, name = project16.name}
    }

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
      runInContextIO U.runMigration requestContext
      runInContextIO TML.runMigration requestContext
      runInContextIO PRJ.runMigration requestContext
      -- WHEN: Call API
      response <- request reqMethod "/api/project-files?tenant=true" reqHeaders reqBody
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
test_403 requestContext = createNoPermissionTest requestContext reqMethod reqUrl [reqCtHeader] reqBody "projects.edit"

test_403_not_member requestContext =
  createNotMemberTest requestContext (runInContextIO U.runMigration requestContext) reqMethod (BS.pack $ "/api/project-files?w=" ++ U.toString secondWorkspaceUuid) reqBody "projects.edit"
