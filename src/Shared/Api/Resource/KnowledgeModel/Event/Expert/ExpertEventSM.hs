module Shared.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.Util.Swagger

instance ToSchema AddExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_eAlbert

instance ToSchema EditExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_eAlbert

instance ToSchema DeleteExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_eNikola
