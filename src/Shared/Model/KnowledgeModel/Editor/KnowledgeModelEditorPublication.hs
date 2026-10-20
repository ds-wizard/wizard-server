module Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorPublication where

import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration

data KnowledgeModelEditorPublication = KnowledgeModelEditorPublication
  { editor :: KnowledgeModelEditor
  , migration :: Maybe KnowledgeModelMigration
  , events :: [KnowledgeModelEditorEvent]
  , previousLocales :: [KnowledgeModelLocale]
  }
