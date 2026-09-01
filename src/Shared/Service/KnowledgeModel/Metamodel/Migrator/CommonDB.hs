module Shared.Service.KnowledgeModel.Metamodel.Migrator.CommonDB where

import qualified Data.Aeson as A
import qualified Data.Vector as Vector

import Shared.Constant.Component
import Shared.Constant.KnowledgeModel
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardInternal
import qualified Shared.Service.KnowledgeModel.Metamodel.Migrator.EventMigrator as EventMigrator
import qualified Shared.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext as EventMigrator
import Shared.Util.List (foldEither)
import Shared.Util.Logger

-- --------------------------------
-- COMMON
-- --------------------------------
migrateEventField entityName createdAt oldMetamodelVersion events callback =
  case events of
    A.Array eventArray ->
      case foldEither $
        EventMigrator.migrate (EventMigrator.MigrationContext createdAt) oldMetamodelVersion knowledgeModelMetamodelVersion
          <$> Vector.toList eventArray of
        Right updatedEvents -> do
          logMigrationMigrationApplied entityName
          callback . A.Array . Vector.fromList . concat $ updatedEvents
        Left error -> logMigrationFailedToConvertToNewMetamodelVersion entityName error
    _ -> logMigrationFailedToMigrateCollection entityName (_ERROR_UTIL_JSON__BAD_FIELD_TYPE "events" "Array")

-- --------------------------------
-- LOGGER
-- --------------------------------
logMigrationStarted entityName = logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ entityName ++ "'): started"

logMigrationMigrationApplied entityName =
  logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ entityName ++ "'): migration applied"

logMigrationCompleted entityName = logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ entityName ++ "'): completed"

logMigrationFailedToConvertToNewMetamodelVersion entityName error = do
  logError _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_NEW_METAMODEL entityName
  logError _CMP_SERVICE . show $ error
  return ()

logMigrationFailedToMigrateCollection entityName error = do
  logError _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_ENTITIES entityName
  logError _CMP_SERVICE . show $ error
  return ()
