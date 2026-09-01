module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorCreateDTO where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorCreate
