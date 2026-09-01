module Shared.Api.Resource.KnowledgeModel.Event.Choice.ChoiceEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.Choice.ChoiceEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.Util.Swagger

instance ToSchema AddChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch3_q11_cho1

instance ToSchema EditChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch3_q11_cho1

instance ToSchema DeleteChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch3_q11_cho1
