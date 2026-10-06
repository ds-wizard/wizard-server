module Shared.Service.Document.DocumentAuditLog where

import Shared.Database.DAO.Project.ProjectDAO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.ProjectAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Submission.Submission
import Shared.Service.AuditLog.AuditLogService
import Shared.Service.DocumentTemplate.DocumentTemplateAuditLog (toDocumentTemplateRef)
import Shared.Service.Project.ProjectAuditLog (toFormatRef, toProjectRef)

al_createDocument :: WizardRequestContextC s m => Document -> Project -> DocumentTemplate -> DocumentTemplateFormatSimple -> m ()
al_createDocument doc project documentTemplate format =
  logAuditLog
    documentsCreate
    (documentEntity doc)
    AuditLogDocumentCreateData {project = toProjectRef project, documentTemplate = toDocumentTemplateRef documentTemplate, format = toFormatRef format}

al_deleteDocument :: WizardRequestContextC s m => Document -> m ()
al_deleteDocument doc = whenAuditLogEnabled $ do
  mProject <- findDocumentProjectRef doc
  logAuditLog documentsDelete (documentEntity doc) AuditLogDocumentData {project = mProject}

al_submitDocument :: WizardRequestContextC s m => Document -> SettingsSubmissionService -> Submission -> m ()
al_submitDocument doc service submission = whenAuditLogEnabled $ do
  mProject <- findDocumentProjectRef doc
  logAuditLog
    documentsSubmit
    (documentEntity doc)
    AuditLogDocumentSubmitData
      { project = mProject
      , service = AuditLogSubmissionServiceRef {id = service.sId, name = service.name}
      , state = submission.state
      , location = submission.location
      }

documentEntity :: Document -> AuditLogEntity
documentEntity doc = entityInWorkspace doc.workspaceUuid doc.uuid doc.name

findDocumentProjectRef :: WizardRequestContextC s m => Document -> m (Maybe AuditLogRef)
findDocumentProjectRef doc = fmap toProjectRef <$> traverse findProjectByUuid doc.projectUuid
