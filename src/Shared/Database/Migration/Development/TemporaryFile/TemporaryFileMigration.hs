module Shared.Database.Migration.Development.TemporaryFile.TemporaryFileMigration where

import Shared.Constant.Component
import Shared.Database.DAO.TemporaryFile.TemporaryFileDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) started"
  deleteTemporaryFiles
  logInfo _CMP_MIGRATION "(TemporaryFile/TemporaryFile) ended"
