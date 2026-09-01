module WizardLib.Public.Database.DAO.Tenant.TenantDAO where

import Data.String
import Database.PostgreSQL.Simple

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.Tenant.TenantSuggestion ()
import WizardLib.Public.Model.Tenant.TenantSuggestion

entityName = "tenant"

pageLabel = "tenants"

findTenantSuggestions :: AppContextC s sc m => Maybe String -> m [TenantSuggestion]
findTenantSuggestions mQuery = do
  table <- tableName entityName
  lookAndFeelTable <- tableName "config_look_and_feel"
  let sql =
        fromString $
          f''
            "SELECT ${tenant}.uuid, \
            \       ${tenant}.name, \
            \       ${tenant}.client_url, \
            \       ${lookAndFeel}.primary_color, \
            \       ${lookAndFeel}.logo_url \
            \FROM ${tenant} \
            \JOIN ${lookAndFeel} ON ${tenant}.uuid = ${lookAndFeel}.tenant_uuid \
            \WHERE ${tenant}.name ~* ? OR ${tenant}.tenant_id ~* ? OR ${tenant}.client_url ~* ? OR (${tenant}.uuid)::text ~* ?"
            [("tenant", table), ("lookAndFeel", lookAndFeelTable)]
  let params = [regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  runDB action
