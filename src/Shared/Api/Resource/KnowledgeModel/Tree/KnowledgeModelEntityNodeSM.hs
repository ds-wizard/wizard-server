module Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelEntitySM ()
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditorEvents
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEntityNodeDTO where
  declareNamedSchema = toSwagger (head amsterdamKnowledgeModelEditorEventsOutline)
