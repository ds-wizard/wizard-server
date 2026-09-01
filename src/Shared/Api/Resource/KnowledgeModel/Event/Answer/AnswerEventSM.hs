module Shared.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Util.Swagger

instance ToSchema AddAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ch1_q2_aNo1

instance ToSchema EditAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ch1_q2_aYes1

instance ToSchema DeleteAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ch1_q2_aYes1
