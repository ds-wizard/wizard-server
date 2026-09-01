module Shared.Api.Resource.TypeHint.TypeHintTestRequestSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.TypeHint.TypeHintTestRequestDTO
import Shared.Api.Resource.TypeHint.TypeHintTestRequestJM ()
import Shared.Database.Migration.Development.TypeHint.Data.TypeHints
import Shared.Util.Swagger

instance ToSchema TypeHintTestRequestDTO where
  declareNamedSchema = toSwagger typeHintTestRequest
