module Shared.Service.Project.File.ProjectFileAuditLog where

import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.ProjectAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.Project
import Shared.Service.AuditLog.AuditLogService
import Shared.Service.Project.ProjectAuditLog (toProjectRef)

al_uploadProjectFile :: WizardRequestContextC s m => Project -> ProjectFile -> m ()
al_uploadProjectFile project projectFile = logAuditLog projectFilesUpload (projectFileEntity project projectFile) (toProjectFileData project)

al_deleteProjectFile :: WizardRequestContextC s m => Project -> ProjectFile -> m ()
al_deleteProjectFile project projectFile = logAuditLog projectFilesDelete (projectFileEntity project projectFile) (toProjectFileData project)

projectFileEntity :: Project -> ProjectFile -> AuditLogEntity
projectFileEntity project projectFile = entityInWorkspace project.workspaceUuid projectFile.uuid projectFile.fileName

toProjectFileData :: Project -> AuditLogProjectFileData
toProjectFileData project = AuditLogProjectFileData {project = toProjectRef project}
