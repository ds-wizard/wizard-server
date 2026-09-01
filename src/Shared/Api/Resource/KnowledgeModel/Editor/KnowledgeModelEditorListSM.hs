module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListSM where

import Data.Swagger

import Shared.Api.Resource.Coordinate.CoordinateSM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Util.Swagger

instance ToSchema KnowledgeModelEditorList where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorList
