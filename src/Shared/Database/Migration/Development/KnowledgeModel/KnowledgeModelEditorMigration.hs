module Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration where

import Data.Foldable (traverse_)

import Shared.Constant.Component
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO (insertKnowledgeModelEvent)
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Service.KnowledgeModel.Editor.EditorService
import Shared.Service.User.WizardUserMapper
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(KnowledgeModel/KnowledgeModelEditor) started"
  deleteKnowledgeModelEditors
  createEditorWithParams
    amsterdamKnowledgeModelEditorList.uuid
    amsterdamKnowledgeModelEditorList.createdAt
    (toDTO userAlbert)
    amsterdamKnowledgeModelEditorCreate
  traverse_ insertKnowledgeModelEvent amsterdamKnowledgeModelEditorEvents
  insertKnowledgeModelEditor differentKnowledgeModelEditor
  logInfo _CMP_MIGRATION "(KnowledgeModel/KnowledgeModelEditor) ended"
