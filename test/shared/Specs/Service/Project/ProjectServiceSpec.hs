module Specs.Service.Project.ProjectServiceSpec where

import Test.Hspec

import Shared.Database.DAO.Project.ProjectDAO
import qualified Shared.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Shared.Database.Migration.Development.Project.Data.Projects
import qualified Shared.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Project.Project
import Shared.Model.User.User
import Shared.Service.Project.ProjectService
import Shared.Util.Uuid

import Specs.Api.Handler.Common
import Specs.Common

projectServiceSpec requestContext =
  describe "Project Service" $ do
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
