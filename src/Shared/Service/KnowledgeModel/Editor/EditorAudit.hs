module Shared.Service.KnowledgeModel.Editor.EditorAudit where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Service.Audit.AuditService

auditKnowledgeModelEditorPublish :: WizardRequestContextC s m => KnowledgeModelEditor -> [KnowledgeModelEditorEvent] -> Maybe Coordinate -> m ()
auditKnowledgeModelEditorPublish kmEditor kmEditorEvents mForkOfPkgId =
  logAuditWithBody
    "knowledge_model_editor"
    "publish"
    (U.toString kmEditor.uuid)
    ( M.fromList
        [ ("kmId", kmEditor.kmId)
        , ("eventSize", show . length $ kmEditorEvents)
        , ("isFork", show . isJust $ mForkOfPkgId)
        ]
    )
