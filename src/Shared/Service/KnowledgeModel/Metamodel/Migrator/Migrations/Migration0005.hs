module Shared.Service.KnowledgeModel.Metamodel.Migrator.Migrations.Migration0005 (
  migrateEventValue,
) where

import Data.Aeson

import Shared.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext

-- Migration #0005 (KM v5 -> v6)
-- (Nothing to be migrated, new type of question "MultiChoice")
migrateEventValue :: MigrationContext -> Value -> Either String [Value]
migrateEventValue _ input = Right [input]
