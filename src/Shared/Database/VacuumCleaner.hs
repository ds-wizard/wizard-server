module Shared.Database.VacuumCleaner where

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Foldable (traverse_)
import Data.String (fromString)
import Database.PostgreSQL.Simple

import Shared.Database.DAO.Common
import Shared.Model.Config.ServerConfig
import Shared.Model.Context.RequestContext
import Shared.Util.Logger
import Shared.Util.String (trim)

runVacuumCleaner :: RequestContextC s sc m => m ()
runVacuumCleaner = do
  context <- ask
  when
    context.serverConfig'.database'.vacuumCleaner.enabled
    (traverse_ runVacuumCleanerForTable context.serverConfig'.database'.vacuumCleaner.tables)

runVacuumCleanerForTable :: RequestContextC s sc m => String -> m ()
runVacuumCleanerForTable tableName = do
  let sql = f' "VACUUM (FULL) %s" [tableName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  return ()
