module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsJM ()
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorEventsDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorEvents'
