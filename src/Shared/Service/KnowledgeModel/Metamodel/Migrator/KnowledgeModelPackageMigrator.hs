module Shared.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelPackageMigrator (
  migrateAll,
) where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import qualified Data.Vector as Vector

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelRawEventJM ()
import Shared.Constant.KnowledgeModel
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Metamodel.Migrator.CommonDB
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

migrateAll :: WizardRequestContextC s m => m ()
migrateAll = do
  logMigrationStarted "knowledge_model_package"
  kmPkgs <- findPackagesByUnsupportedMetamodelVersion knowledgeModelMetamodelVersion
  tenantUuid <- asks (.tenantUuid')
  traverse_ (migrateOneInDB tenantUuid) kmPkgs
  logMigrationCompleted "knowledge_model_package"

-- --------------------------------
-- PRIVATE
-- --------------------------------
migrateOneInDB :: WizardRequestContextC s m => U.UUID -> KnowledgeModelPackage -> m ()
migrateOneInDB tenantUuid pkg = do
  kmEvents <- findPackageRawEvents pkg.uuid
  let events = A.Array . Vector.fromList . fmap (A.toJSON . toRawEvent) $ kmEvents
  migrateEventField "knowledge_model_package" pkg.createdAt pkg.metamodelVersion events $ \eventsMigratedValue -> do
    case A.fromJSON eventsMigratedValue of
      A.Error error -> logMigrationFailedToConvertToNewMetamodelVersion "knowledge_model_package" error
      A.Success eventsMigrated -> do
        let kmEventsMigrated = fmap (toPackageRawEvent pkg.uuid tenantUuid) eventsMigrated
        deletePackageEventsByPackageUuid pkg.uuid
        traverse_ insertPackageRawEvent kmEventsMigrated
        void $ updatePackageMetamodelVersion pkg.uuid knowledgeModelMetamodelVersion
