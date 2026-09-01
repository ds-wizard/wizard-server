module WizardLib.Public.Database.DAO.TemporaryFile.TemporaryFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import WizardLib.Public.Database.Mapping.TemporaryFile.TemporaryFile ()
import WizardLib.Public.Model.TemporaryFile.TemporaryFile

entityName = "temporary_file"

findTemporaryFiles :: AppContextC s sc m => m [TemporaryFile]
findTemporaryFiles = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid]

findTemporaryFilesOlderThen :: AppContextC s sc m => UTCTime -> m [TemporaryFile]
findTemporaryFilesOlderThen date = do
  table <- tableName entityName
  let sql = fromString $ f' "SELECT * FROM %s WHERE expires_at < ? " [table]
  let params = [toField date]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertTemporaryFile :: AppContextC s sc m => TemporaryFile -> m Int64
insertTemporaryFile temporaryFile = do
  table <- tableName entityName
  createInsertFn table temporaryFile

deleteTemporaryFiles :: AppContextC s sc m => m Int64
deleteTemporaryFiles = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deleteTemporaryFileByUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteTemporaryFileByUuid uuid = do
  table <- tableName entityName
  createDeleteEntityByFn table [("uuid", U.toString uuid)]
