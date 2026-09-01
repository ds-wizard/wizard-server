module Shared.Database.Migration.Development.User.RoleMigration where

import Shared.Constant.Component
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Role/Role) started"
  deleteRoles
  insertRole adminRole
  insertRole dataStewardRole
  insertRole researcherRole
  insertRole differentAdminRole
  insertRole differentDataStewardRole
  insertRole differentResearcherRole
  logInfo _CMP_MIGRATION "(Role/Role) ended"
