module Shared.Service.Tenant.Usage.UsageService where

import qualified Data.UUID as U

import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Database.DAO.Tenant.TenantLimitBundleDAO
import Shared.Database.DAO.Tenant.UsageDAO
import Shared.Model.Context.RequestContext
import Shared.Service.Tenant.Usage.UsageMapper

getUsage :: RequestContextC s sc m => U.UUID -> m WizardUsageDTO
getUsage tenantUuid = do
  limitBundle <- findLimitBundleByUuid tenantUuid
  userCount <- countUsersWithTenant tenantUuid
  activeUserCount <- countActiveUsersWithTenant tenantUuid
  knowledgeModelEditorCount <- countKnowledgeModelEditorsWithTenant tenantUuid
  kmCount <- countPackagesWithTenant tenantUuid
  prjCount <- countProjectsWithTenant tenantUuid
  documentTemplateCount <- countDocumentTemplatesWithTenant tenantUuid
  documentTemplateDraftCount <- countDocumentTemplateDraftsWithTenant tenantUuid
  docCount <- countDocumentsWithTenant tenantUuid
  localeCount <- countLocalesWithTenant tenantUuid
  storageCount <- sumStorageWithTenant tenantUuid
  return $ toDTO limitBundle userCount activeUserCount knowledgeModelEditorCount kmCount prjCount documentTemplateCount documentTemplateDraftCount docCount localeCount storageCount
