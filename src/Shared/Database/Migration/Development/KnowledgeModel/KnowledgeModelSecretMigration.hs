module Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretMigration where

import Shared.Constant.Component
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(KnowledgeModelSecret/KnowledgeModelSecret) started"
  deleteKnowledgeModelSecrets
  insertKnowledgeModelSecret kmSecret1
  insertKnowledgeModelSecret kmSecretDifferent
  logInfo _CMP_MIGRATION "(KnowledgeModelSecret/KnowledgeModelSecret) ended"
