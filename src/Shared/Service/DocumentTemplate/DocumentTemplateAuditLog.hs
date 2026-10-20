module Shared.Service.DocumentTemplate.DocumentTemplateAuditLog where

import Control.Monad (when)

import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.LibraryAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Library.LibraryDependents
import Shared.Service.AuditLog.AuditLogService
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAuditLog (toDependentsData)

al_importDocumentTemplate :: WizardRequestContextC s m => Bool -> DocumentTemplate -> m ()
al_importDocumentTemplate fromRegistry dt =
  logAuditLog
    (if fromRegistry then documentTemplatesPullFromRegistry else documentTemplatesImportFromFile)
    (documentTemplateEntity dt)
    (toLibraryData dt)

al_exportDocumentTemplate :: WizardRequestContextC s m => DocumentTemplate -> m ()
al_exportDocumentTemplate dt = logAuditLog documentTemplatesExport (documentTemplateEntity dt) (toLibraryData dt)

al_publishDocumentTemplate :: WizardRequestContextC s m => DocumentTemplate -> m ()
al_publishDocumentTemplate dt =
  logAuditLog
    documentTemplatesPublish
    (documentTemplateEntity dt)
    AuditLogLibraryPublishData {id = dt.id, version = dt.version, editor = AuditLogRef {uuid = dt.uuid, name = dt.name}}

al_modifyDocumentTemplate :: WizardRequestContextC s m => DocumentTemplate -> DocumentTemplate -> m ()
al_modifyDocumentTemplate dt updatedDt =
  when (dt.phase /= updatedDt.phase) $
    case updatedDt.phase of
      DeprecatedDocumentTemplatePhase -> logAuditLog documentTemplatesDeprecate (documentTemplateEntity updatedDt) (toLibraryData updatedDt)
      ReleasedDocumentTemplatePhase -> logAuditLog documentTemplatesRestore (documentTemplateEntity updatedDt) (toLibraryData updatedDt)
      DraftDocumentTemplatePhase -> return ()

al_deleteDocumentTemplate :: WizardRequestContextC s m => DocumentTemplate -> Maybe Bool -> [DocumentTemplate] -> LibraryDependents -> m ()
al_deleteDocumentTemplate dt mAllVersions dts dependents =
  logAuditLog
    documentTemplatesDelete
    (documentTemplateEntity dt)
    AuditLogLibraryDeleteData
      { allVersions = mAllVersions == Just True
      , deletedPackages = fmap (show . createCoordinate) dts
      , dependents = toDependentsData dependents
      }

al_writeDocumentTemplateFiles :: WizardRequestContextC s m => DocumentTemplate -> [String] -> m ()
al_writeDocumentTemplateFiles dt paths = logAuditLog documentTemplateEditorsFilesWrite (documentTemplateEntity dt) AuditLogFilesData {paths = paths}

al_deleteDocumentTemplateFiles :: WizardRequestContextC s m => DocumentTemplate -> [String] -> m ()
al_deleteDocumentTemplateFiles dt paths = logAuditLog documentTemplateEditorsFilesDelete (documentTemplateEntity dt) AuditLogFilesData {paths = paths}

documentTemplateEntity :: DocumentTemplate -> AuditLogEntity
documentTemplateEntity dt = AuditLogEntity {uuid = dt.uuid, name = dt.name, workspaceUuid = dt.workspaceUuid}

toLibraryData :: DocumentTemplate -> AuditLogLibraryData
toLibraryData dt = AuditLogLibraryData {id = dt.id, version = dt.version}

toDocumentTemplateRef :: DocumentTemplate -> AuditLogDocumentTemplateRef
toDocumentTemplateRef dt = AuditLogDocumentTemplateRef {uuid = dt.uuid, id = dt.id, version = dt.version, name = dt.name}
