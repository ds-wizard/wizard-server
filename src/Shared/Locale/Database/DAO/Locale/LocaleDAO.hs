module Shared.Locale.Database.DAO.Locale.LocaleDAO where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.Locale.Database.Mapping.Locale.Locale ()
import Shared.Locale.Database.Mapping.Locale.LocaleSuggestion ()
import Shared.Locale.Model.Locale.Locale
import Shared.Locale.Model.Locale.LocaleSuggestion

entityName = "locale"

pageLabel = "locales"

findLocales :: AppContextC s sc m => m [Locale]
findLocales = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid]

findLocalesByOrganizationIdAndLocaleId :: AppContextC s sc m => String -> String -> m [Locale]
findLocalesByOrganizationIdAndLocaleId organizationId localeId = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntitiesByFn table [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", localeId)]

findLocaleSuggestions :: AppContextC s sc m => Maybe String -> Pageable -> [Sort] -> m (Page LocaleSuggestion)
findLocaleSuggestions mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let condition = "WHERE (organization_id ~* ? OR locale_id ~* ? OR version ~* ? OR name ~* ?) AND enabled = true AND tenant_uuid = ?"
  createFindEntitiesPageableQuerySortFn
    table
    pageLabel
    pageable
    sort
    "uuid, name, description, code, organization_id, locale_id, version, default_locale"
    condition
    [regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery, U.toString tenantUuid]

findLocaleByUuid :: AppContextC s sc m => U.UUID -> m Locale
findLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findLocaleByCoordinate :: AppContextC s sc m => Coordinate -> m Locale
findLocaleByCoordinate Coordinate {..} = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn table [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", entityId), ("version", version)]

findLocaleByCoordinate' :: AppContextC s sc m => Coordinate -> m (Maybe Locale)
findLocaleByCoordinate' Coordinate {..} = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", entityId), ("version", version)]

findLocaleByUuid' :: AppContextC s sc m => U.UUID -> m (Maybe Locale)
findLocaleByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createFindEntityByFn' table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findLocaleSuggestionBy :: AppContextC s sc m => [(String, String)] -> m LocaleSuggestion
findLocaleSuggestionBy queryParams = do
  table <- tableName entityName
  createFindEntityWithFieldsByFn "uuid, name, description, code, organization_id, locale_id, version, default_locale" False table queryParams

countLocalesGroupedByOrganizationIdAndLocaleId :: AppContextC s sc m => m Int
countLocalesGroupedByOrganizationIdAndLocaleId = do
  tenantUuid <- asks (.tenantUuid')
  countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid

countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant :: AppContextC s sc m => U.UUID -> m Int
countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid = do
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT 1 \
            \      FROM %s \
            \      WHERE tenant_uuid = ? \
            \      GROUP BY organization_id, locale_id) nested;"
            [table]
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

updateLocaleByUuid :: AppContextC s sc m => Locale -> m Int64
updateLocaleByUuid locale = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, description = ?, code = ?, organization_id = ?, locale_id = ?, version = ?, default_locale = ?, license = ?, readme = ?, recommended_app_version = ?, enabled = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
            [table]
  let params = toRow locale ++ [toField tenantUuid, toField locale.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetDefaultLocale :: AppContextC s sc m => m ()
unsetDefaultLocale = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql = fromString $ f' "UPDATE %s SET default_locale = false WHERE tenant_uuid = ?" [table]
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

unsetEnabledLocale :: AppContextC s sc m => String -> m ()
unsetEnabledLocale code = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql = fromString $ f' "UPDATE %s SET enabled = false WHERE tenant_uuid = ? AND code = ?" [table]
  let params = [toField tenantUuid, toField code]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

insertLocale :: AppContextC s sc m => Locale -> m Int64
insertLocale locale = do
  table <- tableName entityName
  createInsertFn table locale

deleteLocales :: AppContextC s sc m => m Int64
deleteLocales = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deleteLocaleByUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
