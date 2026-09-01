module Shared.Database.DAO.Tenant.UsageDAO where

import Data.String
import qualified Data.UUID as U
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Model.Context.RequestContext
import Shared.Util.String

countUsersWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countUsersWithTenant tenantUuid = createCountByFn "user_entity" (f' "%s AND machine = false" [tenantCondition]) [U.toString tenantUuid]

countActiveUsersWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countActiveUsersWithTenant tenantUuid = createCountByFn "user_entity" (f' "%s AND machine = false AND active = true" [tenantCondition]) [U.toString tenantUuid]

countKnowledgeModelEditorsWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countKnowledgeModelEditorsWithTenant tenantUuid = createCountByFn "knowledge_model_editor" tenantCondition [U.toString tenantUuid]

countPackagesWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countPackagesWithTenant = countGroupedWithTenant "knowledge_model_package" "" "organization_id, km_id"

countProjectsWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countProjectsWithTenant tenantUuid = createCountByFn "project" tenantCondition [U.toString tenantUuid]

countDocumentTemplatesWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countDocumentTemplatesWithTenant =
  countGroupedWithTenant "document_template" "AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')" "organization_id, template_id"

countDocumentTemplateDraftsWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countDocumentTemplateDraftsWithTenant =
  countGroupedWithTenant "document_template" "AND phase = 'DraftDocumentTemplatePhase'" "organization_id, template_id"

countDocumentsWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countDocumentsWithTenant tenantUuid = createCountByFn "document" tenantCondition [U.toString tenantUuid]

countLocalesWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countLocalesWithTenant = countGroupedWithTenant "locale" "" "organization_id, locale_id"

sumStorageWithTenant :: RequestContextC s sc m => U.UUID -> m Int64
sumStorageWithTenant tenantUuid = do
  docSize <- createSumByFn "document" "file_size" tenantCondition [U.toString tenantUuid]
  templateAssetSize <- createSumByFn "document_template_asset" "file_size" tenantCondition [U.toString tenantUuid]
  projectFileSize <- createSumByFn "project_file" "file_size" tenantCondition [U.toString tenantUuid]
  return $ docSize + templateAssetSize + projectFileSize

countGroupedWithTenant :: RequestContextC s sc m => String -> String -> String -> U.UUID -> m Int
countGroupedWithTenant table condition groupBy tenantUuid = do
  let sql = fromString $ f' "SELECT COUNT(*) FROM (SELECT 1 FROM %s WHERE tenant_uuid = ? %s GROUP BY %s) nested" [table, condition, groupBy]
  createCountWithSqlFn sql [U.toString tenantUuid]
