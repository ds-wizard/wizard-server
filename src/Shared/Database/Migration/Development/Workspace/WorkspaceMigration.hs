module Shared.Database.Migration.Development.Workspace.WorkspaceMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Workspace.Workspace
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Workspace/Workspace) started"
  deleteWorkspaces
  insertWorkspace (defaultWorkspace {defaultRoleUuid = Nothing})
  insertWorkspace (differentWorkspace {defaultRoleUuid = Nothing})
  logInfo _CMP_MIGRATION "(Workspace/Workspace) ended"
