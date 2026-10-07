module Shared.Database.DAO.MailTemplateLocale.MailTemplateLocaleDAO where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.MailTemplateLocale.MailTemplateLocale ()
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.MailTemplateLocale.MailTemplateLocale

entityName = "mail_template_locale"

findMailTemplateLocales :: RequestContextC s sc m => m [MailTemplateLocale]
findMailTemplateLocales = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "name" Ascending]

findMailTemplateLocaleByUuid :: RequestContextC s sc m => U.UUID -> m MailTemplateLocale
findMailTemplateLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findMailTemplateLocaleByCode' :: RequestContextC s sc m => String -> m (Maybe MailTemplateLocale)
findMailTemplateLocaleByCode' code = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("code", code)]

insertMailTemplateLocale :: RequestContextC s sc m => MailTemplateLocale -> m Int64
insertMailTemplateLocale = createInsertFn entityName

updateMailTemplateLocaleByUuid :: RequestContextC s sc m => MailTemplateLocale -> m Int64
updateMailTemplateLocaleByUuid locale = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        "UPDATE mail_template_locale SET uuid = ?, tenant_uuid = ?, code = ?, name = ?, pot_version = ?, enabled = ?, default_locale = ?, official = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow locale ++ [toField tenantUuid, toField locale.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

unsetDefaultMailTemplateLocale :: RequestContextC s sc m => m ()
unsetDefaultMailTemplateLocale = do
  tenantUuid <- asks (.tenantUuid')
  let sql = "UPDATE mail_template_locale SET default_locale = false WHERE tenant_uuid = ? AND default_locale = true"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  void $ runDB action

deleteMailTemplateLocales :: RequestContextC s sc m => m Int64
deleteMailTemplateLocales = createDeleteEntitiesFn entityName

deleteMailTemplateLocaleByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteMailTemplateLocaleByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
