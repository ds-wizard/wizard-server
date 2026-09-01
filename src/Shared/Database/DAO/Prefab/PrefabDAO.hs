module Shared.Database.DAO.Prefab.PrefabDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Prefab.Prefab ()
import Shared.Model.Context.RequestContext
import Shared.Model.Prefab.Prefab
import Shared.Util.String

entityName = "prefab"

findPrefabs :: RequestContextC s sc m => m [Prefab]
findPrefabs = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findPrefabsFiltered :: RequestContextC s sc m => [(String, String)] -> m [Prefab]
findPrefabsFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findPrefabByUuid :: RequestContextC s sc m => U.UUID -> m Prefab
findPrefabByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertPrefab :: RequestContextC s sc m => Prefab -> m Int64
insertPrefab = createInsertFn entityName

updatePrefabByUuid :: RequestContextC s sc m => Prefab -> m Int64
updatePrefabByUuid prefab = do
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, type = ?, name = ?, content = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE uuid = ? AND tenant_uuid = ?"
            [entityName]
  let params = toRow prefab ++ [toField prefab.uuid, toField prefab.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePrefabs :: RequestContextC s sc m => m Int64
deletePrefabs = createDeleteEntitiesFn entityName

deletePrefabByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deletePrefabByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
