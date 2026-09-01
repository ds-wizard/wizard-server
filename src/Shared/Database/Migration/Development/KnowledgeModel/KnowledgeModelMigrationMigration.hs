module Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationMigration where

import Shared.Constant.Component
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) started"
  deleteKnowledgeModelMigrations
  insertKnowledgeModelMigration differentKnowledgeModelMigration
  logInfo _CMP_MIGRATION "(Migration/KnowledgeModel) ended"
