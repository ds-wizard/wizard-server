module Shared.Database.DAO.Websocket.WebsocketOutboxDAO where

import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Constant.Websocket
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Websocket.WebsocketOutbox ()
import Shared.Model.Context.RequestContext
import Shared.Model.Websocket.WebsocketOutbox
import Shared.Util.Logger

entityName = "websocket_outbox"

findWebsocketOutboxByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe WebsocketOutbox)
findWebsocketOutboxByUuid' uuid = do
  let sql = fromString $ f' "SELECT uuid, tenant_uuid, payload FROM %s WHERE uuid = ?" [entityName]
  let params = [toField uuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  case entities of
    [entity] -> return (Just entity)
    _ -> return Nothing

insertWebsocketOutbox :: RequestContextC s sc m => WebsocketOutbox -> m Int64
insertWebsocketOutbox outbox = do
  let sql = fromString $ f' "INSERT INTO %s (uuid, tenant_uuid, payload, created_at) VALUES (?, ?, ?, now())" [entityName]
  let params = toRow outbox
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

deleteOldWebsocketOutboxes :: RequestContextC s sc m => m Int64
deleteOldWebsocketOutboxes = do
  let sql = fromString $ f' "DELETE FROM %s WHERE created_at < now() - interval '%s'" [entityName, websocketOutboxRetentionInterval]
  logQuery sql ([] :: [Action])
  let action conn = execute_ conn sql
  runDB action
