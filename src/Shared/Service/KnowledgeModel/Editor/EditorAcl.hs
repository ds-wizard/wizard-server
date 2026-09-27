module Shared.Service.KnowledgeModel.Editor.EditorAcl where

import qualified Data.UUID as U

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor

checkPermissionToEditor :: WizardRequestContextC s m => U.UUID -> m ()
checkPermissionToEditor editorUuid = do
  checkPermission _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
  editor <- findKnowledgeModelEditorByUuid editorUuid
  checkPermissionInWorkspace _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION (Just editor.workspaceUuid)
