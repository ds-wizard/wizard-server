module Shared.Service.Project.File.ProjectFileAcl where

import qualified Data.UUID as U

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Project
import Shared.Service.Project.ProjectAcl

checkViewPermissionToFile :: WizardRequestContextC s m => U.UUID -> m ()
checkViewPermissionToFile projectUuid = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions

checkEditPermissionToFile :: WizardRequestContextC s m => U.UUID -> m ()
checkEditPermissionToFile projectUuid = do
  project <- findProjectByUuid projectUuid
  checkEditPermissionToProject project.visibility project.sharing project.permissions
