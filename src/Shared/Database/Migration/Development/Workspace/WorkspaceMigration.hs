module Shared.Database.Migration.Development.Workspace.WorkspaceMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Workspace/Workspace) started"
  deleteWorkspaces
  insertWorkspace defaultWorkspace
  insertWorkspace differentWorkspace
  logInfo _CMP_MIGRATION "(Workspace/Workspace) ended"
