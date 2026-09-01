module Shared.Database.DAO.OpenId.OpenIdClientDefinitionDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.OpenId.OpenIdClient ()
import Shared.Database.Mapping.OpenId.OpenIdClientSimple ()
import Shared.Model.Context.RequestContext
import Shared.Model.OpenId.OpenIdClient
import Shared.Model.OpenId.OpenIdClientSimple
import Shared.Util.String

entityName = "openid_client"

findOpenIdClientDefinitions :: RequestContextC s sc m => m [OpenIdClient]
findOpenIdClientDefinitions = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findOpenIdClientDefinitionsSimpleByTenantUuid :: RequestContextC s sc m => U.UUID -> m [OpenIdClientSimple]
findOpenIdClientDefinitionsSimpleByTenantUuid tenantUuid = do
  createFindEntitiesWithFieldsByFn "uuid, name, url, style" entityName [tenantQueryUuid tenantUuid]

findOpenIdClientDefinitionByUuid :: RequestContextC s sc m => U.UUID -> m OpenIdClient
findOpenIdClientDefinitionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findOpenIdClientDefinitionByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe OpenIdClient)
findOpenIdClientDefinitionByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findOpenIdClientDefinitionByUuidAndTenantUuid' :: RequestContextC s sc m => U.UUID -> U.UUID -> m (Maybe OpenIdClient)
findOpenIdClientDefinitionByUuidAndTenantUuid' uuid tenantUuid = do
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertOpenIdClientDefinition :: RequestContextC s sc m => OpenIdClient -> m Int64
insertOpenIdClientDefinition openIdClient = do
  createInsertFn entityName openIdClient

updateOpenIdClientDefinition :: RequestContextC s sc m => OpenIdClient -> m Int64
updateOpenIdClientDefinition openIdClient = do
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, url = ?, client_id = ?, client_secret = ?, parameters = ?, style = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, registration_enabled = ?, scope_profile = ?, scope_email = ? WHERE uuid = ? AND tenant_uuid = ?"
            [entityName]
  let params = toRow openIdClient ++ [toField openIdClient.uuid, toField openIdClient.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteOpenIdClientDefinitionDefinitions :: RequestContextC s sc m => m Int64
deleteOpenIdClientDefinitionDefinitions = do
  createDeleteEntitiesFn entityName

deleteOpenIdClientDefinitionByUuid :: RequestContextC s sc m => U.UUID -> m ()
deleteOpenIdClientDefinitionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()
