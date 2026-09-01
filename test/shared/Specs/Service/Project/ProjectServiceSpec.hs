module Specs.Service.Project.ProjectServiceSpec where

import Control.Monad.Reader (liftIO)
import Test.Hspec

import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as PKG_Migration
import Shared.Database.Migration.Development.Project.Data.ProjectCommands
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Shared.Database.Migration.Development.User.UserMigration as U_Migration
import Shared.Model.PersistentCommand.Project.CreateProjectCommand
import Shared.Model.Project.Project
import Shared.Service.Project.ProjectService

import Specs.Api.Handler.Common
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

compareProject :: Project -> CreateProjectCommand -> IO ()
compareProject project command = liftIO $ do
  project.name `shouldBe` command.name
  project.knowledgeModelPackageUuid `shouldBe` command.knowledgeModelPackageUuid
  project.documentTemplateUuid `shouldBe` command.documentTemplateUuid
  length project.permissions `shouldBe` length command.emails
