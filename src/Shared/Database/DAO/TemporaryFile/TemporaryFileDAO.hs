module Shared.Database.DAO.TemporaryFile.TemporaryFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.TemporaryFile.TemporaryFile ()
import Shared.Model.Context.RequestContext
import Shared.Model.TemporaryFile.TemporaryFile
import Shared.Util.Logger

entityName = "temporary_file"

findTemporaryFiles :: RequestContextC s sc m => m [TemporaryFile]
findTemporaryFiles = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findTemporaryFilesOlderThen :: RequestContextC s sc m => UTCTime -> m [TemporaryFile]
findTemporaryFilesOlderThen date = do
  let sql = fromString $ f' "SELECT * FROM %s WHERE expires_at < ? " [entityName]
  let params = [toField date]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertTemporaryFile :: RequestContextC s sc m => TemporaryFile -> m Int64
insertTemporaryFile temporaryFile = do
  createInsertFn entityName temporaryFile

deleteTemporaryFiles :: RequestContextC s sc m => m Int64
deleteTemporaryFiles = do
  createDeleteEntitiesFn entityName

deleteTemporaryFileByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteTemporaryFileByUuid uuid = do
  createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
