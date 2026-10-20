module Shared.Service.KnowledgeModel.Editor.EditorAuditLog where

import Control.Monad ((<=<))

import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.LibraryAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Service.AuditLog.AuditLogService
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAuditLog

al_createKnowledgeModelEditor :: WizardRequestContextC s m => KnowledgeModelEditor -> m ()
al_createKnowledgeModelEditor editor = whenAuditLogEnabled $ logAuditLog knowledgeModelEditorsCreate (editorEntity editor) =<< toEditorData editor

al_updateKnowledgeModelEditor :: WizardRequestContextC s m => KnowledgeModelEditor -> KnowledgeModelEditor -> m ()
al_updateKnowledgeModelEditor editor updatedEditor =
  logAuditLogChanges
    knowledgeModelEditorsUpdate
    (editorEntity updatedEditor)
    AuditLogEditorChanges
      { name = change editor.name updatedEditor.name
      , id = change editor.id updatedEditor.id
      , version = change editor.version updatedEditor.version
      , description = change editor.description updatedEditor.description
      , license = change editor.license updatedEditor.license
      , language = change editor.language updatedEditor.language
      }

al_deleteKnowledgeModelEditor :: WizardRequestContextC s m => KnowledgeModelEditor -> m ()
al_deleteKnowledgeModelEditor editor = whenAuditLogEnabled $ logAuditLog knowledgeModelEditorsDelete (editorEntity editor) =<< toEditorData editor

al_addKnowledgeModelEditorEvents :: WizardRequestContextC s m => KnowledgeModelEditor -> Int -> m ()
al_addKnowledgeModelEditorEvents editor count = logAuditLog knowledgeModelEditorsEventsAdd (editorEntity editor) AuditLogEventCountData {events = count}

al_setKnowledgeModelEditorReplies :: WizardRequestContextC s m => KnowledgeModelEditor -> Int -> m ()
al_setKnowledgeModelEditorReplies editor count = logAuditLog knowledgeModelEditorsRepliesSet (editorEntity editor) AuditLogReplyCountData {replies = count}

al_startKnowledgeModelMigration :: WizardRequestContextC s m => KnowledgeModelEditor -> KnowledgeModelMigration -> m ()
al_startKnowledgeModelMigration editor = whenAuditLogEnabled . (logAuditLog knowledgeModelEditorsMigrationStart (editorEntity editor) <=< toMigrationData)

al_finishKnowledgeModelMigration :: WizardRequestContextC s m => KnowledgeModelEditor -> KnowledgeModelMigration -> m ()
al_finishKnowledgeModelMigration editor = whenAuditLogEnabled . (logAuditLog knowledgeModelEditorsMigrationFinish (editorEntity editor) <=< toMigrationData)

al_cancelKnowledgeModelMigration :: WizardRequestContextC s m => KnowledgeModelEditor -> KnowledgeModelMigration -> m ()
al_cancelKnowledgeModelMigration editor = whenAuditLogEnabled . (logAuditLog knowledgeModelEditorsMigrationCancel (editorEntity editor) <=< toMigrationData)

editorEntity :: KnowledgeModelEditor -> AuditLogEntity
editorEntity editor = entityInWorkspace editor.workspaceUuid editor.uuid editor.name

toEditorData :: WizardRequestContextC s m => KnowledgeModelEditor -> m AuditLogKnowledgeModelEditorData
toEditorData editor = do
  mPreviousPkg <- traverse findPackageByUuid editor.previousPackageUuid
  return AuditLogKnowledgeModelEditorData {id = editor.id, version = editor.version, basedOn = fmap toKnowledgeModelRef mPreviousPkg}

toMigrationData :: WizardRequestContextC s m => KnowledgeModelMigration -> m AuditLogKnowledgeModelMigrationData
toMigrationData migration = do
  sourcePkg <- findPackageByUuid migration.editorPreviousPackageUuid
  targetPkg <- findPackageByUuid migration.targetPackageUuid
  return AuditLogKnowledgeModelMigrationData {sourcePackage = toKnowledgeModelRef sourcePkg, targetPackage = toKnowledgeModelRef targetPkg}
