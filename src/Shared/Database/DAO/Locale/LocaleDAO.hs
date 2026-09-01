module Shared.Database.DAO.Locale.LocaleDAO where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Locale.Locale ()
import Shared.Database.Mapping.Locale.LocaleSuggestion ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Locale.Locale
import Shared.Model.Locale.LocaleSuggestion
import Shared.Util.String

entityName = "locale"

pageLabel = "locales"

findLocales :: RequestContextC s sc m => m [Locale]
findLocales = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findLocalesByOrganizationIdAndLocaleId :: RequestContextC s sc m => String -> String -> m [Locale]
findLocalesByOrganizationIdAndLocaleId organizationId localeId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", localeId)]

findLocaleSuggestions :: RequestContextC s sc m => Maybe String -> Pageable -> [Sort] -> m (Page LocaleSuggestion)
findLocaleSuggestions mQuery pageable sort = do
  tenantUuid <- asks (.tenantUuid')
  let condition = "WHERE (organization_id ~* ? OR locale_id ~* ? OR version ~* ? OR name ~* ?) AND enabled = true AND tenant_uuid = ?"
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, name, description, code, organization_id, locale_id, version, default_locale"
    condition
    [regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery, U.toString tenantUuid]

findLocaleByUuid :: RequestContextC s sc m => U.UUID -> m Locale
findLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findLocaleByCoordinate :: RequestContextC s sc m => Coordinate -> m Locale
findLocaleByCoordinate Coordinate {..} = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", entityId), ("version", version)]

findLocaleByCoordinate' :: RequestContextC s sc m => Coordinate -> m (Maybe Locale)
findLocaleByCoordinate' Coordinate {..} = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("locale_id", entityId), ("version", version)]

findLocaleByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe Locale)
findLocaleByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findLocaleSuggestionBy :: RequestContextC s sc m => [(String, String)] -> m LocaleSuggestion
findLocaleSuggestionBy queryParams = do
  createFindEntityWithFieldsByFn "uuid, name, description, code, organization_id, locale_id, version, default_locale" False entityName queryParams

countLocalesGroupedByOrganizationIdAndLocaleId :: RequestContextC s sc m => m Int
countLocalesGroupedByOrganizationIdAndLocaleId = do
  tenantUuid <- asks (.tenantUuid')
  countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid

countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countLocalesGroupedByOrganizationIdAndLocaleIdWithTenant tenantUuid = do
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT 1 \
            \      FROM %s \
            \      WHERE tenant_uuid = ? \
            \      GROUP BY organization_id, locale_id) nested;"
            [entityName]
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

updateLocaleByUuid :: RequestContextC s sc m => Locale -> m Int64
updateLocaleByUuid locale = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "UPDATE %s SET uuid = ?, name = ?, description = ?, code = ?, organization_id = ?, locale_id = ?, version = ?, default_locale = ?, license = ?, readme = ?, recommended_app_version = ?, enabled = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
            [entityName]
  let params = toRow locale ++ [toField tenantUuid, toField locale.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetDefaultLocale :: RequestContextC s sc m => m ()
unsetDefaultLocale = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "UPDATE %s SET default_locale = false WHERE tenant_uuid = ?" [entityName]
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

unsetEnabledLocale :: RequestContextC s sc m => String -> m ()
unsetEnabledLocale code = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "UPDATE %s SET enabled = false WHERE tenant_uuid = ? AND code = ?" [entityName]
  let params = [toField tenantUuid, toField code]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

insertLocale :: RequestContextC s sc m => Locale -> m Int64
insertLocale locale = do
  createInsertFn entityName locale

deleteLocales :: RequestContextC s sc m => m Int64
deleteLocales = do
  createDeleteEntitiesFn entityName

deleteLocaleByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
