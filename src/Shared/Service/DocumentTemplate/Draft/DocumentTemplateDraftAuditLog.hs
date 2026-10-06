module Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftAuditLog where

import qualified Data.UUID as U

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.LibraryAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.AuditLog.AuditLogService
import Shared.Service.DocumentTemplate.DocumentTemplateAuditLog

al_createDocumentTemplateDraft :: WizardRequestContextC s m => DocumentTemplate -> Maybe U.UUID -> m ()
al_createDocumentTemplateDraft draft mBasedOnUuid = whenAuditLogEnabled $ do
  mBasedOn <- traverse findDocumentTemplateByUuid mBasedOnUuid
  logAuditLog
    documentTemplateEditorsCreate
    (documentTemplateEntity draft)
    AuditLogDocumentTemplateEditorData {id = draft.id, version = draft.version, basedOn = fmap toDocumentTemplateRef mBasedOn}

al_updateDocumentTemplateDraft :: WizardRequestContextC s m => DocumentTemplate -> DocumentTemplate -> m ()
al_updateDocumentTemplateDraft draft updatedDraft =
  logAuditLogChanges
    documentTemplateEditorsUpdate
    (documentTemplateEntity updatedDraft)
    AuditLogEditorChanges
      { name = change draft.name updatedDraft.name
      , id = change draft.id updatedDraft.id
      , version = change draft.version updatedDraft.version
      , description = change draft.description updatedDraft.description
      , license = change draft.license updatedDraft.license
      , language = change draft.language updatedDraft.language
      }

al_deleteDocumentTemplateDraft :: WizardRequestContextC s m => DocumentTemplate -> m ()
al_deleteDocumentTemplateDraft draft = logAuditLog documentTemplateEditorsDelete (documentTemplateEntity draft) (toLibraryData draft)
