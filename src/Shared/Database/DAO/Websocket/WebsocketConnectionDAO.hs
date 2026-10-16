module Shared.Database.DAO.Websocket.WebsocketConnectionDAO where

import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Constant.Websocket
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Websocket.WebsocketConnection ()
import Shared.Model.Context.RequestContext
import Shared.Model.Websocket.WebsocketConnection
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketSubscription
import Shared.Util.Logger

entityName = "websocket_connection"

columns = "uuid, node_uuid, tenant_uuid, subscription, entity_uuid, user_uuid, websocket_perm, user_group_uuids, online_user"

findWebsocketConnectionsByEntity :: RequestContextC s sc m => U.UUID -> WebsocketSubscription -> U.UUID -> m [WebsocketConnection]
findWebsocketConnectionsByEntity tenantUuid subscription entityUuid = do
  let sql = fromString $ f' "SELECT %s FROM %s WHERE tenant_uuid = ? AND subscription = ? AND entity_uuid = ? ORDER BY created_at" [columns, entityName]
  let params = [toField tenantUuid, toField subscription, toField entityUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

countWebsocketConnections :: RequestContextC s sc m => m Int64
countWebsocketConnections = createCountFn entityName

insertWebsocketConnection :: RequestContextC s sc m => WebsocketConnection -> m Int64
insertWebsocketConnection connection = do
  let sql = fromString $ f' "INSERT INTO %s (%s, created_at, heartbeat_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, now(), now())" [entityName, columns]
  let params = toRow connection
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

updateWebsocketConnectionPermByUuid :: RequestContextC s sc m => U.UUID -> WebsocketPerm -> m Int64
updateWebsocketConnectionPermByUuid uuid websocketPerm = do
  let sql = fromString $ f' "UPDATE %s SET websocket_perm = ? WHERE uuid = ?" [entityName]
  let params = [toField websocketPerm, toField uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

removeUserGroupFromWebsocketConnections :: RequestContextC s sc m => U.UUID -> U.UUID -> U.UUID -> [U.UUID] -> m [(U.UUID, U.UUID)]
removeUserGroupFromWebsocketConnections nodeUuid tenantUuid userGroupUuid userUuids = do
  let sql =
        fromString $
          f'
            "UPDATE %s \
            \SET user_group_uuids = array_remove(user_group_uuids, ?), \
            \    online_user = jsonb_set(online_user, '{groupUuids}', (online_user -> 'groupUuids') - ?::text) \
            \WHERE node_uuid = ? AND tenant_uuid = ? AND subscription = ? AND user_uuid = ANY(?::uuid[]) AND ? = ANY(user_group_uuids) \
            \RETURNING uuid, entity_uuid"
            [entityName]
  let params =
        [ toField userGroupUuid
        , toField (U.toText userGroupUuid)
        , toField nodeUuid
        , toField tenantUuid
        , toField ProjectWebsocketSubscription
        , toField userUuids
        , toField userGroupUuid
        ]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

updateWebsocketConnectionHeartbeat :: RequestContextC s sc m => U.UUID -> [U.UUID] -> m [U.UUID]
updateWebsocketConnectionHeartbeat nodeUuid uuids = do
  let sql = fromString $ f' "UPDATE %s SET heartbeat_at = now() WHERE node_uuid = ? AND uuid = ANY(?::uuid[]) RETURNING uuid" [entityName]
  let params = [toField nodeUuid, toField uuids]
  logQuery sql params
  let action conn = query conn sql params
  fmap fromOnly <$> runDB action

deleteWebsocketConnectionByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteWebsocketConnectionByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]

deleteWebsocketConnectionsByNodeUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteWebsocketConnectionsByNodeUuid nodeUuid = createDeleteEntitiesByFn entityName [("node_uuid", U.toString nodeUuid)]

deleteStaleWebsocketConnections :: RequestContextC s sc m => m [(U.UUID, WebsocketSubscription, U.UUID)]
deleteStaleWebsocketConnections = do
  let sql = fromString $ f' "DELETE FROM %s WHERE heartbeat_at < now() - interval '%s' RETURNING tenant_uuid, subscription, entity_uuid" [entityName, websocketStaleConnectionInterval]
  logQuery sql ([] :: [Action])
  let action conn = query_ conn sql
  runDB action
