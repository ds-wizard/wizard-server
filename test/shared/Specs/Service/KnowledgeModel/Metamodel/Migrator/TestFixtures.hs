module Specs.Service.KnowledgeModel.Metamodel.Migrator.TestFixtures where

import Shared.Service.KnowledgeModel.Metamodel.Migrator.Migrations.MigrationContext

defaultContext :: MigrationContext
defaultContext = MigrationContext {ctxCreatedAt = read "2022-01-01 12:00:00.000000 UTC"}
