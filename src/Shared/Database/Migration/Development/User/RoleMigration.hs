module Shared.Database.Migration.Development.User.RoleMigration where

import Shared.Constant.Component
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Workspace.Workspace
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Role/Role) started"
  updateWorkspaceByUuid (defaultWorkspace {defaultRoleUuid = Nothing} :: Workspace)
  updateWorkspaceByUuid (differentWorkspace {defaultRoleUuid = Nothing} :: Workspace)
  deleteRoles
  insertRole adminRole
  insertRole dataStewardRole
  insertRole researcherRole
  insertRole differentAdminRole
  insertRole differentDataStewardRole
  insertRole differentResearcherRole
  insertRole defaultWorkspaceAdminRole
  insertRole defaultWorkspaceUserRole
  insertRole differentWorkspaceAdminRole
  insertRole differentWorkspaceUserRole
  updateWorkspaceByUuid defaultWorkspace
  updateWorkspaceByUuid differentWorkspace
  logInfo _CMP_MIGRATION "(Role/Role) ended"
