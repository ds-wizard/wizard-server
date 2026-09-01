module Shared.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Shared.Util.Swagger

instance ToSchema AddPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_mtrF

instance ToSchema EditPhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_mtrF

instance ToSchema DeletePhaseEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_mtrF
