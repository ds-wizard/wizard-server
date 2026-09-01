module Shared.Bootstrap.DatabaseMigration where

import Database.PostgreSQL.Migration.Migration

import Shared.Constant.Component
import Shared.Util.Logger

runDBMigration serverContext prodDBMigrations runDevDBMigrations =
  if serverContext.serverConfig.database.useDevMigration
    then do
      (Right result) <- runDevDBMigrations serverContext
      return result
    else runLogging serverContext.serverConfig.logging.level $ migrateDatabase serverContext.dbPool prodDBMigrations (logInfo _CMP_MIGRATION)
