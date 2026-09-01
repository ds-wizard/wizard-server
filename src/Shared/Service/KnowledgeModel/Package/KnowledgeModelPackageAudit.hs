module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Project.Project
import Shared.Service.Audit.AuditService

auditPackageFailedToDelete :: WizardRequestContextC s m => U.UUID -> String -> String -> m ()
auditPackageFailedToDelete entityId reasonType reasonId =
  logAuditWithBody "package" "failedToDelete" (U.toString entityId) (M.fromList [("reasonType", reasonType), ("reasonId", reasonId)])

auditPackageFailedToDeleteDuePreviousPackages :: WizardRequestContextC s m => U.UUID -> [KnowledgeModelPackage] -> m ()
auditPackageFailedToDeleteDuePreviousPackages entityId pkgs =
  auditPackageFailedToDelete entityId "PreviousPackage" (show $ fmap (.uuid) pkgs)

auditPackageFailedToDeleteDueParentPackages :: WizardRequestContextC s m => U.UUID -> [KnowledgeModelPackage] -> m ()
auditPackageFailedToDeleteDueParentPackages entityId pkgs =
  auditPackageFailedToDelete entityId "ParentPackage" (show $ fmap (.uuid) pkgs)

auditPackageFailedToDeleteDueKmEditors :: WizardRequestContextC s m => U.UUID -> [KnowledgeModelEditor] -> m ()
auditPackageFailedToDeleteDueKmEditors entityId knowledgeModelEditors =
  auditPackageFailedToDelete entityId "Knowledge Model Editor" (show $ fmap (\b -> U.toString b.uuid) knowledgeModelEditors)

auditPackageFailedToDeleteDueProjects :: WizardRequestContextC s m => U.UUID -> [Project] -> m ()
auditPackageFailedToDeleteDueProjects entityId projects =
  auditPackageFailedToDelete entityId "Knowledge Model Editor" (show $ fmap (\project -> U.toString project.uuid) projects)
