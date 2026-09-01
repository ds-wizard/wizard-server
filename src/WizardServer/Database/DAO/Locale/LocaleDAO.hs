module WizardServer.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Locale.Locale ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Locale.Locale
import Shared.Model.Locale.LocaleSimple
import WizardServer.Database.Mapping.Locale.LocaleList ()
import WizardServer.Database.Mapping.Locale.LocaleSimple ()
import WizardServer.Model.Locale.LocaleList

entityName = "locale"

pageLabel = "locales"

findLocales :: WizardRequestContextC s m => m [Locale]
findLocales = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findLocalesPage :: WizardRequestContextC s m => Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> m (Page LocaleList)
findLocalesPage mOrganizationId mLocaleId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    "registry_locale"
    pageLabel
    pageable
    sort
    "locale.uuid, locale.name, locale.description, locale.code, locale.organization_id, locale.locale_id, locale.version, locale.default_locale, locale.enabled, registry_locale.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, locale.created_at, locale.updated_at"
    "locale_id"
    mQuery
    Nothing
    mOrganizationId
    mLocaleId
    Nothing
    ""

findLocalesFiltered :: WizardRequestContextC s m => [(String, String)] -> m [Locale]
findLocalesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findLocalesFilteredWithTenant :: WizardRequestContextC s m => U.UUID -> [(String, String)] -> m [Locale]
findLocalesFilteredWithTenant tenantUuid queryParams = createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findLocalesByCodeWithTenant :: WizardRequestContextC s m => U.UUID -> String -> String -> m [LocaleSimple]
findLocalesByCodeWithTenant tenantUuid code shortCode = do
  let sql =
        fromString
          "SELECT uuid, name, code, default_locale \
          \FROM locale \
          \WHERE tenant_uuid = ? \
          \  AND enabled = true \
          \  AND (code = ? \
          \    OR code = ? \
          \    OR default_locale = true);"
  let params = [toField . U.toString $ tenantUuid, toField code, toField shortCode]
  logQuery sql params
  let action conn = query conn sql params
  runDB action
