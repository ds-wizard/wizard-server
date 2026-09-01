module Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Util.Swagger

instance ToSchema AddKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1

instance ToSchema EditKnowledgeModelEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1
