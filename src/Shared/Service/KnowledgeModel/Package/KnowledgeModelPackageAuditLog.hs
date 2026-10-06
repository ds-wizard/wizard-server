module Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageAuditLog where

import Control.Monad (when)

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.LibraryAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Library.LibraryDependents
import Shared.Service.AuditLog.AuditLogService

al_importKnowledgeModel :: WizardRequestContextC s m => Bool -> KnowledgeModelPackageSimpleDTO -> m ()
al_importKnowledgeModel fromRegistry pkg =
  logAuditLog
    (if fromRegistry then knowledgeModelsPullFromRegistry else knowledgeModelsImportFromFile)
    AuditLogEntity {uuid = pkg.uuid, name = pkg.name, workspaceUuid = pkg.workspaceUuid}
    AuditLogLibraryData {id = pkg.id, version = pkg.version}

al_exportKnowledgeModel :: WizardRequestContextC s m => KnowledgeModelPackage -> m ()
al_exportKnowledgeModel pkg = logAuditLog knowledgeModelsExport (packageEntity pkg) (toLibraryData pkg)

al_publishKnowledgeModel :: WizardRequestContextC s m => KnowledgeModelEditor -> KnowledgeModelPackage -> m ()
al_publishKnowledgeModel kmEditor pkg =
  logAuditLog
    knowledgeModelsPublish
    (packageEntity pkg)
    AuditLogLibraryPublishData {id = pkg.id, version = pkg.version, editor = AuditLogRef {uuid = kmEditor.uuid, name = kmEditor.name}}

al_modifyKnowledgeModel :: WizardRequestContextC s m => KnowledgeModelPackage -> KnowledgeModelPackage -> m ()
al_modifyKnowledgeModel pkg updatedPkg = do
  when (pkg.phase /= updatedPkg.phase) $
    logAuditLog (phaseDefinition updatedPkg.phase) (packageEntity updatedPkg) (toLibraryData updatedPkg)
  logAuditLogUpdate
    knowledgeModelsSetVisibility
    (packageEntity updatedPkg)
    AuditLogKnowledgeModelVisibilityChanges {public = change pkg.public updatedPkg.public}
    (toLibraryData updatedPkg)
  where
    phaseDefinition DeprecatedKnowledgeModelPackagePhase = knowledgeModelsDeprecate
    phaseDefinition ReleasedKnowledgeModelPackagePhase = knowledgeModelsRestore

al_deleteKnowledgeModel :: WizardRequestContextC s m => KnowledgeModelPackage -> Maybe Bool -> [KnowledgeModelPackage] -> LibraryDependents -> m ()
al_deleteKnowledgeModel pkg mAllVersions pkgs dependents =
  logAuditLog
    knowledgeModelsDelete
    (packageEntity pkg)
    AuditLogLibraryDeleteData
      { allVersions = mAllVersions == Just True
      , deletedPackages = fmap (show . createCoordinate) pkgs
      , dependents = toDependentsData dependents
      }

packageEntity :: KnowledgeModelPackage -> AuditLogEntity
packageEntity pkg = AuditLogEntity {uuid = pkg.uuid, name = pkg.name, workspaceUuid = pkg.workspaceUuid}

toLibraryData :: KnowledgeModelPackage -> AuditLogLibraryData
toLibraryData pkg = AuditLogLibraryData {id = pkg.id, version = pkg.version}

toKnowledgeModelRef :: KnowledgeModelPackage -> AuditLogKnowledgeModelRef
toKnowledgeModelRef pkg = AuditLogKnowledgeModelRef {uuid = pkg.uuid, id = pkg.id, version = pkg.version, name = pkg.name}

toDependentsData :: LibraryDependents -> AuditLogLibraryDependentsData
toDependentsData dependents =
  AuditLogLibraryDependentsData
    { editors = length dependents.editors
    , projects = length dependents.projects
    , documents = length dependents.documents
    }
