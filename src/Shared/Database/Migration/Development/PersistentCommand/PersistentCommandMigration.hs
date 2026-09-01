module Shared.Database.Migration.Development.PersistentCommand.PersistentCommandMigration where

import Shared.Constant.Component
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Model.Context.RequestContext
import Shared.Util.Logger

runMigration :: RequestContextC s sc m => m ()
runMigration = do
  logInfoI _CMP_MIGRATION "(PersistentCommand/PersistentCommand) started"
  deletePersistentCommands
  logInfoI _CMP_MIGRATION "(PersistentCommand/PersistentCommand) ended"
