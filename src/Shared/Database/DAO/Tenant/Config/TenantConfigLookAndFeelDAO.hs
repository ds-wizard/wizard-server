module Shared.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.Tenant.Config.TenantConfigLookAndFeel ()
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.Tenant.Config.TenantConfig
import Shared.Util.String

findTenantConfigLookAndFeel :: RequestContextC s sc m => m TenantConfigLookAndFeel
findTenantConfigLookAndFeel = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigLookAndFeelByUuid tenantUuid

findTenantConfigLookAndFeelByUuid :: RequestContextC s sc m => U.UUID -> m TenantConfigLookAndFeel
findTenantConfigLookAndFeelByUuid tenantUuid = do
  let entityName = "config_look_and_feel"
  let menuLinkTable = "config_look_and_feel_custom_menu_link"
  config <- createFindEntityByFn entityName [("tenant_uuid", U.toString tenantUuid)]
  customMenuLinks <- createFindEntitiesBySortedFn menuLinkTable [("tenant_uuid", U.toString tenantUuid)] [Sort "position" Ascending]
  return $ config {customMenuLinks = customMenuLinks}

insertTenantConfigLookAndFeel :: RequestContextC s sc m => TenantConfigLookAndFeel -> m Int64
insertTenantConfigLookAndFeel config = do
  let entityName = "config_look_and_feel"
  createInsertFn entityName config

insertTenantConfigLookAndFeelCustomMenuLink :: RequestContextC s sc m => TenantConfigLookAndFeelCustomMenuLink -> m Int64
insertTenantConfigLookAndFeelCustomMenuLink customMenuLink = do
  let menuLinkTable = "config_look_and_feel_custom_menu_link"
  createInsertFn menuLinkTable customMenuLink

updateTenantConfigLookAndFeel :: RequestContextC s sc m => TenantConfigLookAndFeel -> m Int64
updateTenantConfigLookAndFeel config = do
  let entityName = "config_look_and_feel"
  let menuLinkTable = "config_look_and_feel_custom_menu_link"
  let sql =
        fromString $
          f''
            "UPDATE ${lookAndFeel} SET tenant_uuid = ?, app_title = ?, app_title_short = ?, logo_url = ?, primary_color = ?, illustrations_color = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?; \
            \DELETE FROM ${menuLink} WHERE tenant_uuid = ?;"
            [("lookAndFeel", entityName), ("menuLink", menuLinkTable)]
            ++ concatMap (const (f' "INSERT INTO %s VALUES (?, ?, ?, ?, ?, ?, ?, ?);" [menuLinkTable])) config.customMenuLinks
  let params =
        toRow config
          ++ [toField config.tenantUuid, toField config.tenantUuid]
          ++ concatMap toRow config.customMenuLinks
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigLookAndFeels :: RequestContextC s sc m => m Int64
deleteTenantConfigLookAndFeels = do
  let entityName = "config_look_and_feel"
  let menuLinkTable = "config_look_and_feel_custom_menu_link"
  createDeleteEntitiesFn menuLinkTable
  createDeleteEntitiesFn entityName
