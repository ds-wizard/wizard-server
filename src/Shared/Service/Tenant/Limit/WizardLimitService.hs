module Shared.Service.Tenant.Limit.WizardLimitService where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U
import GHC.Int

import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Database.DAO.Document.DocumentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Project.ProjectDAO
import Shared.Database.DAO.Project.ProjectFileDAO
import Shared.Database.DAO.Tenant.TenantLimitBundleDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Tenant.Limit.TenantLimitBundle
import Shared.Model.Tenant.Limit.TenantLimitBundleChange
import Shared.Service.Tenant.Limit.LimitMapper
import Shared.Service.Tenant.Limit.LimitService
import Shared.Service.Tenant.Usage.WizardUsageService

createLimitBundle :: WizardRequestContextC s m => U.UUID -> UTCTime -> m TenantLimitBundle
createLimitBundle tenantUuid now = do
  let limitBundle = fromCreate tenantUuid now
  insertLimitBundle limitBundle
  return limitBundle

modifyLimitBundle :: WizardRequestContextC s m => U.UUID -> TenantLimitBundleChange -> m WizardUsageDTO
modifyLimitBundle tenantUuid reqDto =
  runInTransaction $ do
    checkPermission _TENANTS_MANAGE_ROLE_PERMISSION
    limitBundle <- findLimitBundleByUuid tenantUuid
    now <- liftIO getCurrentTime
    let limitBundleUpdated = fromChangeDTO limitBundle reqDto now
    updateLimitBundleByUuid limitBundleUpdated
    getUsage tenantUuid

checkUserLimit :: WizardRequestContextC s m => m ()
checkUserLimit = do
  limit <- findCurrentTenantLimits
  count <- countUsers
  checkLimit "users" count limit.users

checkUserLimitForTenant :: WizardRequestContextC s m => U.UUID -> m ()
checkUserLimitForTenant tenantUuid = do
  limit <- findTenantLimits tenantUuid
  count <- countUsersWithTenant tenantUuid
  checkLimit "users" count limit.users

checkActiveUserLimit :: WizardRequestContextC s m => m ()
checkActiveUserLimit = do
  limit <- findCurrentTenantLimits
  count <- countActiveUsers
  checkLimit "active users" count limit.activeUsers

checkActiveUserLimitForTenant :: WizardRequestContextC s m => U.UUID -> m ()
checkActiveUserLimitForTenant tenantUuid = do
  limit <- findTenantLimits tenantUuid
  count <- countActiveUsersWithTenant tenantUuid
  checkLimit "active users" count limit.activeUsers

checkKnowledgeModelEditorLimit :: WizardRequestContextC s m => m ()
checkKnowledgeModelEditorLimit = do
  limit <- findCurrentTenantLimits
  count <- countKnowledgeModelEditors
  checkLimit "knowledgeModelEditors" count limit.knowledgeModelEditors

checkPackageLimit :: WizardRequestContextC s m => String -> m ()
checkPackageLimit pkgId = do
  existingPackages <- findPackagesFiltered [("id", pkgId)]
  when (null existingPackages) $ do
    limit <- findCurrentTenantLimits
    count <- countPackagesGroupedById
    checkLimit "knowledge models" count limit.knowledgeModels

checkProjectLimit :: WizardRequestContextC s m => m ()
checkProjectLimit = checkProjectsLimit 1

checkProjectsLimit :: WizardRequestContextC s m => Int -> m ()
checkProjectsLimit newCount = do
  limit <- findCurrentTenantLimits
  count <- countProjects
  checkLimit "projects" (count + newCount - 1) limit.projects

checkDocumentTemplateLimit :: WizardRequestContextC s m => String -> m ()
checkDocumentTemplateLimit dtId = do
  existingTemplates <- findDocumentTemplatesFiltered [("id", dtId)]
  when (all (\dt -> dt.phase == DraftDocumentTemplatePhase) existingTemplates) $ do
    limit <- findCurrentTenantLimits
    count <- countDocumentTemplatesGroupedById
    checkLimit "document templates" count limit.documentTemplates

checkDocumentTemplateDraftLimit :: WizardRequestContextC s m => m ()
checkDocumentTemplateDraftLimit = do
  limit <- findCurrentTenantLimits
  count <- countDraftsGroupedById
  checkLimit "document template drafts" count limit.documentTemplateDrafts

checkDocumentLimit :: WizardRequestContextC s m => m ()
checkDocumentLimit = do
  limit <- findCurrentTenantLimits
  count <- countDocuments
  checkLimit "documents" count limit.documents

checkStorageSize :: WizardRequestContextC s m => Int64 -> m ()
checkStorageSize newFileSize = do
  limit <- findCurrentTenantLimits
  docSize <- sumDocumentFileSize
  templateAssetSize <- sumAssetFileSize
  projectFileSize <- sumProjectFileSize
  let storageCount = docSize + templateAssetSize + projectFileSize
  checkLimit "storage" (storageCount + newFileSize) limit.storage

findCurrentTenantLimits :: WizardRequestContextC s m => m TenantLimitBundleChange
findCurrentTenantLimits = findTenantLimits =<< asks (.tenantUuid')
