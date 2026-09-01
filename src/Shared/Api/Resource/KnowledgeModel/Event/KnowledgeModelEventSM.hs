module Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Choice.ChoiceEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Metric.MetricEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Move.MoveEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Question.QuestionEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Reference.ReferenceEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventSM ()
import Shared.Api.Resource.KnowledgeModel.Event.Tag.TagEventSM ()
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent

instance ToSchema KnowledgeModelEvent where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema KnowledgeModelEventData where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
