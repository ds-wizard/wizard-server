module Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Control.Monad.Reader (ask)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Integration.Aws.Lambda
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.Logger
import Shared.Common.Util.String (trim)
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.LambdaInvocationResult ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommand ()
import Shared.PersistentCommand.Database.Mapping.PersistentCommand.PersistentCommandSimple ()
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper

entityName = "persistent_command"

channelName = "persistent_command_channel"

findPersistentCommands :: (AppContextC s sc m, FromField identity) => m [PersistentCommand identity]
findPersistentCommands = do
  table <- tableName entityName
  createFindEntitiesFn table

findPersistentCommandsForRetryByStates :: (AppContextC s sc m, FromField identity) => m [PersistentCommandSimple identity]
findPersistentCommandsForRetryByStates = do
  table <- tableName entityName
  let sql =
        f'
          "SELECT uuid, destination, component, tenant_uuid, created_by \
          \FROM %s \
          \WHERE (state = 'NewPersistentCommandState' \
          \  OR (state = 'ErrorPersistentCommandState' AND attempts < max_attempts AND updated_at < (now() - (2 ^ attempts - 1) * INTERVAL '1 min'))) \
          \  AND internal = true \
          \ORDER BY created_at \
          \LIMIT 5 \
          \FOR UPDATE"
          [table]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

findPersistentCommandByUuid :: (AppContextC s sc m, FromField identity) => U.UUID -> m (PersistentCommand identity)
findPersistentCommandByUuid uuid = do
  table <- tableName entityName
  createFindEntityWithFieldsByFn "*" True table [("uuid", U.toString uuid)]

findPersistentCommandByUuid' :: (AppContextC s sc m, FromField identity) => U.UUID -> m (Maybe (PersistentCommand identity))
findPersistentCommandByUuid' uuid = do
  table <- tableName entityName
  createFindEntityWithFieldsByFn' "*" table [("uuid", U.toString uuid)]

findPersistentCommandSimpleByUuid :: (AppContextC s sc m, FromField identity) => U.UUID -> m (PersistentCommandSimple identity)
findPersistentCommandSimpleByUuid uuid = do
  table <- tableName entityName
  createFindEntityWithFieldsByFn "uuid, destination, tenant_uuid, created_by" False table [("uuid", U.toString uuid)]

insertPersistentCommand :: (AppContextC s sc m, ToField identity) => PersistentCommand identity -> m Int64
insertPersistentCommand command = do
  table <- tableName entityName
  createInsertFn table command
  context <- ask
  case (command.internal, L.find (\lf -> lf.component == command.component) context.serverConfig'.persistentCommand'.lambdaFunctions) of
    (True, _) -> notifyPersistentCommandQueue
    (False, Nothing) -> notifySpecificPersistentCommandQueue command
    (False, Just lf) -> invokeLambdaFunction (toSimple command) lf

updatePersistentCommandByUuid :: (AppContextC s sc m, ToField identity) => PersistentCommand identity -> m Int64
updatePersistentCommandByUuid command = do
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, state = ?, component = ?, function = ?, body = ?, last_error_message = ?, attempts = ?, max_attempts = ?, tenant_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, internal = ?, destination = ?, last_trace_uuid = ? WHERE uuid = ? AND tenant_uuid = ? AND state != 'DonePersistentCommandState'"
            [table]
  let params = toRow command ++ [toField command.uuid, toField command.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePersistentCommands :: AppContextC s sc m => m Int64
deletePersistentCommands = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deletePersistentCommandsByCreatedBy :: AppContextC s sc m => [U.UUID] -> m Int64
deletePersistentCommandsByCreatedBy createdBys = do
  table <- tableName entityName
  createDeleteEntityWhereInFn table "created_by" (fmap U.toString createdBys)

deletePersistentCommandByUuid :: AppContextC s sc m => U.UUID -> m Int64
deletePersistentCommandByUuid uuid = do
  table <- tableName entityName
  createDeleteEntityByFn table [("uuid", U.toString uuid)]

listenPersistentCommandChannel :: AppContextC s sc m => m ()
listenPersistentCommandChannel = createChannelListener channelName

createChannelListener :: AppContextC s sc m => String -> m ()
createChannelListener name = do
  let sql = f' "LISTEN %s" [name]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action
  logInfoI _CMP_DATABASE (f' "Listening for '%s' channel" [name])

getChannelNotification :: AppContextC s sc m => m Notification
getChannelNotification = do
  logInfoI _CMP_DATABASE "Waiting for new notification"
  notification <- runDB getNotification
  logInfoI _CMP_DATABASE (f' "Receiving notification for channel '%s'" [BS.unpack . notificationChannel $ notification])
  return notification

notifyPersistentCommandQueue :: AppContextC s sc m => m Int64
notifyPersistentCommandQueue = do
  let sql = f' "NOTIFY %s" [channelName]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

notifySpecificPersistentCommandQueue :: AppContextC s sc m => PersistentCommand identity -> m Int64
notifySpecificPersistentCommandQueue command = do
  let sql = f' "NOTIFY %s__%s, '%s'" [channelName, command.component, U.toString command.uuid]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = execute_ conn (fromString sql)
  runDB action

invokeLambdaFunction :: AppContextC s sc m => PersistentCommandSimple identity -> ServerConfigPersistentCommandLambda -> m Int64
invokeLambdaFunction command lf = do
  invokeLambda lf.functionArn "{}"
  return 1
