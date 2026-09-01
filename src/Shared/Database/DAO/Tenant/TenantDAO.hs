module Shared.Database.DAO.Tenant.TenantDAO where

import Data.String
import Database.PostgreSQL.Simple

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Tenant.TenantSuggestion ()
import Shared.Model.Context.RequestContext
import Shared.Model.Tenant.TenantSuggestion
import Shared.Util.String

entityName = "tenant"

pageLabel = "tenants"

findTenantSuggestions :: RequestContextC s sc m => Maybe String -> m [TenantSuggestion]
findTenantSuggestions mQuery = do
  let lookAndFeelTable = "config_look_and_feel"
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
            [("tenant", entityName), ("lookAndFeel", lookAndFeelTable)]
  let params = [regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery]
  logQuery sql params
  let action conn = query conn sql params
  runDB action
