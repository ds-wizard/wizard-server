module Specs.Service.Project.ProjectServiceSpec where

import Control.Monad.Reader (liftIO)
import Test.Hspec

import Shared.Constant.Workspace
import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as PKG_Migration
import Shared.Database.Migration.Development.Project.Data.ProjectCommands
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.User.Data.WizardUsers
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.Project.CreateProjectCommand
import Shared.Model.Project.Project
import Shared.Model.User.User
import Shared.Service.Project.ProjectService
import Shared.Util.Uuid

import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

projectServiceSpec requestContext =
  describe "Project Service" $ do
    it "createProjectsFromCommands" $
      -- GIVEN:
      do
        runInContextIO U_Migration.runMigration requestContext
        runInContextIO PKG_Migration.runMigration requestContext
        runInContextIO TML_Migration.runMigration requestContext
        -- WHEN:
        (Right ()) <- runInContext (createProjectsFromCommands [command1, command2]) requestContext
        -- THEN:
        (Right projects) <- runInContext findProjects requestContext
        length projects `shouldBe` 2
        compareProject (head projects) command1
        compareProject (projects !! 1) command2

    it "createProjectsFromCommands requires a workspace in a multi-workspace tenant" $
      -- GIVEN:
      do
        runInContextIO U_Migration.runMigration requestContext
        runInContextIO PKG_Migration.runMigration requestContext
        runInContextIO TML_Migration.runMigration requestContext
        enableMultiWorkspace requestContext
        -- WHEN:
        result <- runInContext (createProjectsFromCommands [command1]) requestContext
        -- THEN:
        result `shouldBe` Left (UserError _ERROR_SERVICE_WORKSPACE__WORKSPACE_REQUIRED)
        assertCountInDB findProjects requestContext 0

    it "cleanProjects works" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        assertCountInDB findProjects requestContext 3
        -- WHEN:
        (Right ()) <- runInContext cleanProjects requestContext
        -- THEN:
        assertCountInDB findProjects requestContext 2

    it "cleanProjects keeps a project without ACL whose creator exists" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration requestContext
        runInContextIO PRJ_Migration.runMigration requestContext
        runInContextIO (insertProject projectWithoutAclCreatedByAlbert) requestContext
        assertCountInDB findProjects requestContext 4
        -- WHEN:
        (Right ()) <- runInContext cleanProjects requestContext
        -- THEN:
        (Right projects) <- runInContext findProjects requestContext
        fmap (.uuid) projects `shouldContain` [projectWithoutAclCreatedByAlbert.uuid]
        fmap (.uuid) projects `shouldNotContain` [project3.uuid]

projectWithoutAclCreatedByAlbert :: Project
projectWithoutAclCreatedByAlbert = project3 {uuid = u' "b3c1d2e4-5f60-4a7b-8c9d-0e1f2a3b4c5d", creatorUuid = Just userAlbert.uuid} :: Project

compareProject :: Project -> CreateProjectCommand -> IO ()
compareProject project command = liftIO $ do
  project.name `shouldBe` command.name
  project.knowledgeModelPackageUuid `shouldBe` command.knowledgeModelPackageUuid
  project.documentTemplateUuid `shouldBe` command.documentTemplateUuid
  project.workspaceUuid `shouldBe` defaultWorkspaceUuid
  length project.permissions `shouldBe` length command.emails
