module Shared.Api.Resource.KnowledgeModel.Event.Move.MoveEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Move.MoveEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Util.Swagger

instance ToSchema MoveQuestionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q1__to_ch2

instance ToSchema MoveAnswerEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q2_aYes__to_ch2_q3

instance ToSchema MoveChoiceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch3_q11_cho1__to_ch3_q12

instance ToSchema MoveExpertEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q2_eAlbert__to_ch2_q3

instance ToSchema MoveReferenceEvent where
  declareNamedSchema = toSwaggerWithType "eventType" m_km1_ch1_q2_r1__to_ch2_q3
