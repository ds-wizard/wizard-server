module Shared.Model.Document.PreviewSource where

import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.Project.Project

data PreviewSource
  = ProjectPreviewSource Project
  | KnowledgeModelEditorPreviewSource KnowledgeModelEditor
  deriving (Show)
