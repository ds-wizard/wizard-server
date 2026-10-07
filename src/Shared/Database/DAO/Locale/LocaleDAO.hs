module Shared.Database.DAO.Locale.LocaleDAO where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Locale.Locale ()
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.Locale.Locale

entityName = "locale"

findLocales :: RequestContextC s sc m => m [Locale]
findLocales = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "name" Ascending]

findEnabledLocales :: RequestContextC s sc m => m [Locale]
findEnabledLocales = do
  tenantUuid <- asks (.tenantUuid')
  findEnabledLocalesWithTenant tenantUuid

findEnabledLocalesWithTenant :: RequestContextC s sc m => U.UUID -> m [Locale]
findEnabledLocalesWithTenant tenantUuid =
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid, ("enabled", "true")] [Sort "name" Ascending]

findLocaleByUuid :: RequestContextC s sc m => U.UUID -> m Locale
findLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findLocaleByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe Locale)
findLocaleByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findLocaleByCode' :: RequestContextC s sc m => String -> m (Maybe Locale)
findLocaleByCode' code = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("code", code)]

insertLocale :: RequestContextC s sc m => Locale -> m Int64
insertLocale = createInsertFn entityName

updateLocaleByUuid :: RequestContextC s sc m => Locale -> m Int64
updateLocaleByUuid locale = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        "UPDATE locale SET uuid = ?, tenant_uuid = ?, code = ?, name = ?, pot_version = ?, enabled = ?, default_locale = ?, official = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow locale ++ [toField tenantUuid, toField locale.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetDefaultLocale :: RequestContextC s sc m => m ()
unsetDefaultLocale = do
  tenantUuid <- asks (.tenantUuid')
  let sql = "UPDATE locale SET default_locale = false WHERE tenant_uuid = ? AND default_locale = true"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

deleteLocales :: RequestContextC s sc m => m Int64
deleteLocales = createDeleteEntitiesFn entityName

deleteLocaleByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
