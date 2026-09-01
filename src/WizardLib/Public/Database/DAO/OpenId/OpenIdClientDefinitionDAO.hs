module WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.OpenId.OpenIdClient ()
import WizardLib.Public.Database.Mapping.OpenId.OpenIdClientSimple ()
import WizardLib.Public.Model.OpenId.OpenIdClient
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

entityName = "openid_client"

findOpenIdClientDefinitions :: AppContextC s sc m => m [OpenIdClient]
findOpenIdClientDefinitions = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid]

findOpenIdClientDefinitionsSimpleByTenantUuid :: AppContextC s sc m => U.UUID -> m [OpenIdClientSimple]
findOpenIdClientDefinitionsSimpleByTenantUuid tenantUuid = do
  table <- tableName entityName
  createFindEntitiesWithFieldsByFn "uuid, name, url, style" table [tenantQueryUuid tenantUuid]

findOpenIdClientDefinitionByUuid :: AppContextC s sc m => U.UUID -> m OpenIdClient
findOpenIdClientDefinitionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findOpenIdClientDefinitionByUuid' :: AppContextC s sc m => U.UUID -> m (Maybe OpenIdClient)
findOpenIdClientDefinitionByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findOpenIdClientDefinitionByUuidAndTenantUuid' :: AppContextC s sc m => U.UUID -> U.UUID -> m (Maybe OpenIdClient)
findOpenIdClientDefinitionByUuidAndTenantUuid' uuid tenantUuid = do
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertOpenIdClientDefinition :: AppContextC s sc m => OpenIdClient -> m Int64
insertOpenIdClientDefinition openIdClient = do
  table <- tableName entityName
  createInsertFn table openIdClient

updateOpenIdClientDefinition :: AppContextC s sc m => OpenIdClient -> m Int64
updateOpenIdClientDefinition openIdClient = do
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, url = ?, client_id = ?, client_secret = ?, parameters = ?, style = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, registration_enabled = ?, scope_profile = ?, scope_email = ? WHERE uuid = ? AND tenant_uuid = ?"
            [table]
  let params = toRow openIdClient ++ [toField openIdClient.uuid, toField openIdClient.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteOpenIdClientDefinitionDefinitions :: AppContextC s sc m => m Int64
deleteOpenIdClientDefinitionDefinitions = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deleteOpenIdClientDefinitionByUuid :: AppContextC s sc m => U.UUID -> m ()
deleteOpenIdClientDefinitionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
  return ()
