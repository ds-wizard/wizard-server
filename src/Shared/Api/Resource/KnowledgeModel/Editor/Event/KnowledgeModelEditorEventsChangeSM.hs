module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeSM where

import Data.Swagger

import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorEventsChangeDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorEventsChange

instance ToSchema KnowledgeModelEditorEventChangeDTO where
  declareNamedSchema = toSwagger (head amsterdamKnowledgeModelEditorEventsChange.events)
