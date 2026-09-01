module Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishService (
  publishPackageFromKnowledgeModelEditor,
  publishPackageFromMigration,
) where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.KnowledgeModel.Editor.Collaboration.CollaborationService
import Shared.Service.KnowledgeModel.Editor.EditorAudit
import Shared.Service.KnowledgeModel.Editor.EditorMapper
import Shared.Service.KnowledgeModel.Editor.EditorUtil
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService
import Shared.Service.KnowledgeModel.Migration.KnowledgeModelMigrationAudit
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishMapper
import Shared.Service.KnowledgeModel.Publish.KnowledgeModelPublishValidation
import Shared.Service.KnowledgeModel.Squash.Squasher
import Shared.Util.Uuid

publishPackageFromKnowledgeModelEditor :: WizardRequestContextC s m => PackagePublishEditorDTO -> m KnowledgeModelPackageSimpleDTO
publishPackageFromKnowledgeModelEditor reqDto = do
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    validateMigrationExistence reqDto.editorUuid
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.editorUuid
    kmEditorEvents <- findKnowledgeModelEventsByEditorUuid reqDto.editorUuid
    let kmEvents = fmap toKnowledgeModelEvent kmEditorEvents
    mForkOfPkgId <- getEditorForkOfPackageId kmEditor
    mMergeCheckpointPkgId <- getEditorMergeCheckpointPackageId kmEditor
    auditKnowledgeModelEditorPublish kmEditor kmEditorEvents mForkOfPkgId
    doPublishPackage
      kmEditor.version
      kmEditor
      kmEvents
      kmEditor.description
      kmEditor.readme
      mForkOfPkgId
      mMergeCheckpointPkgId
      reqDto.localeUuids

publishPackageFromMigration :: WizardRequestContextC s m => PackagePublishMigrationDTO -> m KnowledgeModelPackageSimpleDTO
publishPackageFromMigration reqDto = do
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
    kmEditor <- findKnowledgeModelEditorByUuid reqDto.editorUuid
    ms <- findKnowledgeModelMigrationByEditorUuid reqDto.editorUuid
    deleteKnowledgeModelMigrationByEditorUuid reqDto.editorUuid
    auditKmMigrationFinish reqDto.editorUuid
    mForkOfPkg <- findPackageByUuid ms.targetPackageUuid
    let mForkOfPkgId = Just $ createCoordinate mForkOfPkg
    editorPreviousPackage <- findPackageByUuid ms.editorPreviousPackageUuid
    let mMergeCheckpointPkgId = Just $ Coordinate {organizationId = editorPreviousPackage.organizationId, entityId = editorPreviousPackage.kmId, version = reqDto.version}
    doPublishPackage
      reqDto.version
      kmEditor
      ms.resultEvents
      reqDto.description
      reqDto.readme
      mForkOfPkgId
      mMergeCheckpointPkgId
      reqDto.localeUuids

-- --------------------------------
-- PRIVATE
-- --------------------------------
doPublishPackage
  :: WizardRequestContextC s m
  => String
  -> KnowledgeModelEditor
  -> [KnowledgeModelEvent]
  -> String
  -> String
  -> Maybe Coordinate
  -> Maybe Coordinate
  -> Maybe [U.UUID]
  -> m KnowledgeModelPackageSimpleDTO
doPublishPackage version kmEditor kmEvents description readme mForkOfPkgId mMergeCheckpointPkgId mLocaleUuids = do
  let squashedKmEvents = squash kmEvents
  tcOrganization <- findTenantConfigOrganization
  validateNewPackageVersion version kmEditor tcOrganization
  uuid <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let (pkg, pkgEvents) = fromPackage kmEditor uuid mForkOfPkgId mMergeCheckpointPkgId tcOrganization version description readme squashedKmEvents now
  createdPkg <- createPackage (pkg, pkgEvents)
  copyLocalesForPublishedPackage mLocaleUuids kmEditor.previousPackageUuid pkg.uuid
  let updatedKmEditor = kmEditor {previousPackageUuid = Just pkg.uuid, updatedAt = now} :: KnowledgeModelEditor
  updateKnowledgeModelEditorByUuid updatedKmEditor
  deleteKnowledgeModelEventsByEditorUuid kmEditor.uuid
  logOutOnlineUsersWhenKnowledgeModelEditorDramaticallyChanged kmEditor.uuid
  return createdPkg
