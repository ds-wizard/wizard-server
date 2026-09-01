module Shared.Service.KnowledgeModel.Metamodel.MigrationService where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.WizardRequestContext
import Shared.Model.PersistentCommand.Migration.Metamodel.MigrateToLatestMetamodelVersionCommand
import Shared.Model.Tenant.Tenant
import qualified Shared.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelBundleMigrator as KnowledgeModelBundleMigrator
import qualified Shared.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelEditorMigrator as KnowledgeModelEditorMigrator
import qualified Shared.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelPackageMigrator as KnowledgeModelPackageMigrator
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Util.JSON
import Shared.Util.Uuid

migrateToLatestMetamodelVersionCommand :: WizardRequestContextC s m => Tenant -> Maybe U.UUID -> m ()
migrateToLatestMetamodelVersionCommand tenant mCreatedBy = do
  uuid <- liftIO generateUuid
  let dto = MigrateToLatestMetamodelVersionCommand {tenantUuid = tenant.uuid}
  let body = encodeJsonToString dto
  now <- liftIO getCurrentTime
  let command = toPersistentCommand uuid "metamodel_migrator" "migrate" body 10 tenant.uuid (fmap U.toString mCreatedBy) now
  insertPersistentCommand command
  void $ updateTenantByUuid (tenant {state = HousekeepingInProgressTenantState, updatedAt = now})

migrateKnowledgeModelBundle :: WizardRequestContextC s m => Value -> m Value
migrateKnowledgeModelBundle value =
  runInTransaction $
    let eResult = KnowledgeModelBundleMigrator.migrate value
     in case eResult of
          Right result -> return result
          Left error -> throwError error

migrateTenant :: WizardRequestContextC s m => m ()
migrateTenant =
  runInTransaction $ do
    KnowledgeModelPackageMigrator.migrateAll
    KnowledgeModelEditorMigrator.migrateAll
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    tenant <- findTenantByUuid tenantUuid
    void $ updateTenantByUuid (tenant {state = ReadyForUseTenantState, updatedAt = now})
