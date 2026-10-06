module WizardServer.Database.Migration.Production.Migration where

import Database.PostgreSQL.Migration.Entity

import qualified WizardServer.Database.Migration.Production.Migration_4_35_0.Migration as M_4_35_0
import qualified WizardServer.Database.Migration.Production.Migration_5_0_0.Migration as M_5_0_0

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions =
  [ M_4_35_0.definition
  , M_5_0_0.definition
  ]
