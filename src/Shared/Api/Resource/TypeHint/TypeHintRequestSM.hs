module Shared.Api.Resource.TypeHint.TypeHintRequestSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.TypeHint.TypeHintRequestDTO
import Shared.Api.Resource.TypeHint.TypeHintRequestJM ()
import Shared.Database.Migration.Development.TypeHint.Data.TypeHints
import Shared.Util.Swagger

instance ToSchema TypeHintRequestDTO where
  declareNamedSchema = toSwaggerWithType "requestType" projectTypeHintRequest

instance ToSchema KnowledgeModelEditorIntegrationTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" kmEditorIntegrationTypeHintRequest'

instance ToSchema KnowledgeModelEditorQuestionTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" kmEditorQuestionTypeHintRequest'

instance ToSchema ProjectTypeHintRequest where
  declareNamedSchema = toSwaggerWithType "requestType" projectTypeHintRequest'
