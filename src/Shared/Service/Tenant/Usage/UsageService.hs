module Shared.Service.Tenant.Usage.UsageService where

import qualified Data.UUID as U

import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Database.DAO.Tenant.UsageDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Tenant.Usage.UsageMapper

getUsage :: WizardRequestContextC s m => U.UUID -> m WizardUsageDTO
getUsage tenantUuid = do
  limits <- findTenantLimits tenantUuid
  userCount <- countUsersWithTenant tenantUuid
  activeUserCount <- countActiveUsersWithTenant tenantUuid
  knowledgeModelEditorCount <- countKnowledgeModelEditorsWithTenant tenantUuid
  kmCount <- countPackagesWithTenant tenantUuid
  prjCount <- countProjectsWithTenant tenantUuid
  documentTemplateCount <- countDocumentTemplatesWithTenant tenantUuid
  documentTemplateDraftCount <- countDocumentTemplateDraftsWithTenant tenantUuid
  docCount <- countDocumentsWithTenant tenantUuid
  storageCount <- sumStorageWithTenant tenantUuid
  return $ toDTO limits userCount activeUserCount knowledgeModelEditorCount kmCount prjCount documentTemplateCount documentTemplateDraftCount docCount storageCount
