module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorChangeDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorChange
