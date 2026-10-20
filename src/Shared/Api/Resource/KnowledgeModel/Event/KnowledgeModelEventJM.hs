module Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM where

import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Proxy

import Shared.Api.Resource.KnowledgeModel.Event.Answer.AnswerEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Chapter.ChapterEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Choice.ChoiceEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Expert.ExpertEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Metric.MetricEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Move.MoveEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Phase.PhaseEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Question.QuestionEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Reference.ReferenceEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Tag.TagEventJM ()
import Shared.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.Model.KnowledgeModel.Event.Choice.ChoiceEvent
import Shared.Model.KnowledgeModel.Event.Expert.ExpertEvent
import Shared.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Event.Metric.MetricEvent
import Shared.Model.KnowledgeModel.Event.Move.MoveEvent
import Shared.Model.KnowledgeModel.Event.Phase.PhaseEvent
import Shared.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Shared.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Shared.Model.KnowledgeModel.Event.Tag.TagEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON KnowledgeModelEvent where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEventData where
  toJSON (AddKnowledgeModelEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddKnowledgeModelEvent")]) (toJSON event)
  toJSON (EditKnowledgeModelEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditKnowledgeModelEvent")]) (toJSON event)
  toJSON (AddChapterEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddChapterEvent")]) (toJSON event)
  toJSON (EditChapterEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditChapterEvent")]) (toJSON event)
  toJSON (DeleteChapterEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteChapterEvent")]
  toJSON (AddQuestionEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddQuestionEvent")]) (toJSON event)
  toJSON (EditQuestionEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditQuestionEvent")]) (toJSON event)
  toJSON (DeleteQuestionEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteQuestionEvent")]
  toJSON (AddAnswerEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddAnswerEvent")]) (toJSON event)
  toJSON (EditAnswerEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditAnswerEvent")]) (toJSON event)
  toJSON (DeleteAnswerEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteAnswerEvent")]
  toJSON (AddChoiceEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddChoiceEvent")]) (toJSON event)
  toJSON (EditChoiceEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditChoiceEvent")]) (toJSON event)
  toJSON (DeleteChoiceEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteChoiceEvent")]
  toJSON (AddExpertEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddExpertEvent")]) (toJSON event)
  toJSON (EditExpertEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditExpertEvent")]) (toJSON event)
  toJSON (DeleteExpertEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteExpertEvent")]
  toJSON (AddReferenceEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddReferenceEvent")]) (toJSON event)
  toJSON (EditReferenceEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditReferenceEvent")]) (toJSON event)
  toJSON (DeleteReferenceEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteReferenceEvent")]
  toJSON (AddTagEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddTagEvent")]) (toJSON event)
  toJSON (EditTagEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditTagEvent")]) (toJSON event)
  toJSON (DeleteTagEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteTagEvent")]
  toJSON (AddIntegrationEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddIntegrationEvent")]) (toJSON event)
  toJSON (EditIntegrationEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditIntegrationEvent")]) (toJSON event)
  toJSON (DeleteIntegrationEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteIntegrationEvent")]
  toJSON (AddMetricEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddMetricEvent")]) (toJSON event)
  toJSON (EditMetricEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditMetricEvent")]) (toJSON event)
  toJSON (DeleteMetricEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteMetricEvent")]
  toJSON (AddPhaseEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddPhaseEvent")]) (toJSON event)
  toJSON (EditPhaseEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditPhaseEvent")]) (toJSON event)
  toJSON (DeletePhaseEvent' event) = Object . KM.fromList $ [("eventType", String "DeletePhaseEvent")]
  toJSON (AddResourceCollectionEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddResourceCollectionEvent")]) (toJSON event)
  toJSON (EditResourceCollectionEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditResourceCollectionEvent")]) (toJSON event)
  toJSON (DeleteResourceCollectionEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteResourceCollectionEvent")]
  toJSON (AddResourcePageEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "AddResourcePageEvent")]) (toJSON event)
  toJSON (EditResourcePageEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "EditResourcePageEvent")]) (toJSON event)
  toJSON (DeleteResourcePageEvent' event) = Object . KM.fromList $ [("eventType", String "DeleteResourcePageEvent")]
  toJSON (MoveQuestionEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "MoveQuestionEvent")]) (toJSON event)
  toJSON (MoveAnswerEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "MoveAnswerEvent")]) (toJSON event)
  toJSON (MoveChoiceEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "MoveChoiceEvent")]) (toJSON event)
  toJSON (MoveExpertEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "MoveExpertEvent")]) (toJSON event)
  toJSON (MoveReferenceEvent' event) = mergeValue (Object . KM.fromList $ [("eventType", String "MoveReferenceEvent")]) (toJSON event)

instance FromJSON KnowledgeModelEventData where
  parseJSON (Object o) = do
    eventType <- o .: "eventType"
    case eventType of
      "AddKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (AddKnowledgeModelEvent' event)
      "EditKnowledgeModelEvent" -> parseJSON (Object o) >>= \event -> return (EditKnowledgeModelEvent' event)
      "AddChapterEvent" -> parseJSON (Object o) >>= \event -> return (AddChapterEvent' event)
      "EditChapterEvent" -> parseJSON (Object o) >>= \event -> return (EditChapterEvent' event)
      "DeleteChapterEvent" -> return $ DeleteChapterEvent' DeleteChapterEvent
      "AddQuestionEvent" -> parseJSON (Object o) >>= \event -> return (AddQuestionEvent' event)
      "EditQuestionEvent" -> parseJSON (Object o) >>= \event -> return (EditQuestionEvent' event)
      "DeleteQuestionEvent" -> return $ DeleteQuestionEvent' DeleteQuestionEvent
      "AddAnswerEvent" -> parseJSON (Object o) >>= \event -> return (AddAnswerEvent' event)
      "EditAnswerEvent" -> parseJSON (Object o) >>= \event -> return (EditAnswerEvent' event)
      "DeleteAnswerEvent" -> return $ DeleteAnswerEvent' DeleteAnswerEvent
      "AddChoiceEvent" -> parseJSON (Object o) >>= \event -> return (AddChoiceEvent' event)
      "EditChoiceEvent" -> parseJSON (Object o) >>= \event -> return (EditChoiceEvent' event)
      "DeleteChoiceEvent" -> return $ DeleteChoiceEvent' DeleteChoiceEvent
      "AddExpertEvent" -> parseJSON (Object o) >>= \event -> return (AddExpertEvent' event)
      "EditExpertEvent" -> parseJSON (Object o) >>= \event -> return (EditExpertEvent' event)
      "DeleteExpertEvent" -> return $ DeleteExpertEvent' DeleteExpertEvent
      "AddReferenceEvent" -> parseJSON (Object o) >>= \event -> return (AddReferenceEvent' event)
      "EditReferenceEvent" -> parseJSON (Object o) >>= \event -> return (EditReferenceEvent' event)
      "DeleteReferenceEvent" -> return $ DeleteReferenceEvent' DeleteReferenceEvent
      "AddTagEvent" -> parseJSON (Object o) >>= \event -> return (AddTagEvent' event)
      "EditTagEvent" -> parseJSON (Object o) >>= \event -> return (EditTagEvent' event)
      "DeleteTagEvent" -> return $ DeleteTagEvent' DeleteTagEvent
      "AddIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (AddIntegrationEvent' event)
      "EditIntegrationEvent" -> parseJSON (Object o) >>= \event -> return (EditIntegrationEvent' event)
      "DeleteIntegrationEvent" -> return $ DeleteIntegrationEvent' DeleteIntegrationEvent
      "AddMetricEvent" -> parseJSON (Object o) >>= \event -> return (AddMetricEvent' event)
      "EditMetricEvent" -> parseJSON (Object o) >>= \event -> return (EditMetricEvent' event)
      "DeleteMetricEvent" -> return $ DeleteMetricEvent' DeleteMetricEvent
      "AddPhaseEvent" -> parseJSON (Object o) >>= \event -> return (AddPhaseEvent' event)
      "EditPhaseEvent" -> parseJSON (Object o) >>= \event -> return (EditPhaseEvent' event)
      "DeletePhaseEvent" -> return $ DeletePhaseEvent' DeletePhaseEvent
      "AddResourceCollectionEvent" -> parseJSON (Object o) >>= \event -> return (AddResourceCollectionEvent' event)
      "EditResourceCollectionEvent" -> parseJSON (Object o) >>= \event -> return (EditResourceCollectionEvent' event)
      "DeleteResourceCollectionEvent" -> return $ DeleteResourceCollectionEvent' DeleteResourceCollectionEvent
      "AddResourcePageEvent" -> parseJSON (Object o) >>= \event -> return (AddResourcePageEvent' event)
      "EditResourcePageEvent" -> parseJSON (Object o) >>= \event -> return (EditResourcePageEvent' event)
      "DeleteResourcePageEvent" -> return $ DeleteResourcePageEvent' DeleteResourcePageEvent
      "MoveQuestionEvent" -> parseJSON (Object o) >>= \event -> return (MoveQuestionEvent' event)
      "MoveAnswerEvent" -> parseJSON (Object o) >>= \event -> return (MoveAnswerEvent' event)
      "MoveChoiceEvent" -> parseJSON (Object o) >>= \event -> return (MoveChoiceEvent' event)
      "MoveExpertEvent" -> parseJSON (Object o) >>= \event -> return (MoveExpertEvent' event)
      "MoveReferenceEvent" -> parseJSON (Object o) >>= \event -> return (MoveReferenceEvent' event)
      unsupportedEventType -> fail ("One of the events has unsupported eventType: " ++ unsupportedEventType)
  parseJSON _ = mzero

instance JsonSchema KnowledgeModelEventData where
  jsonSchema _ = oneOfSchema (fmap snd knowledgeModelEventSchemas)

knowledgeModelEventTypes :: [String]
knowledgeModelEventTypes = fmap fst knowledgeModelEventSchemas

knowledgeModelEventSchemas :: [(String, Value)]
knowledgeModelEventSchemas =
  [ eventSchema "AddKnowledgeModelEvent" (Proxy :: Proxy AddKnowledgeModelEvent)
  , eventSchema "EditKnowledgeModelEvent" (Proxy :: Proxy EditKnowledgeModelEvent)
  , eventSchema "AddChapterEvent" (Proxy :: Proxy AddChapterEvent)
  , eventSchema "EditChapterEvent" (Proxy :: Proxy EditChapterEvent)
  , deleteEventSchema "DeleteChapterEvent"
  , eventSchema "AddQuestionEvent" (Proxy :: Proxy AddQuestionEvent)
  , eventSchema "EditQuestionEvent" (Proxy :: Proxy EditQuestionEvent)
  , deleteEventSchema "DeleteQuestionEvent"
  , eventSchema "AddAnswerEvent" (Proxy :: Proxy AddAnswerEvent)
  , eventSchema "EditAnswerEvent" (Proxy :: Proxy EditAnswerEvent)
  , deleteEventSchema "DeleteAnswerEvent"
  , eventSchema "AddChoiceEvent" (Proxy :: Proxy AddChoiceEvent)
  , eventSchema "EditChoiceEvent" (Proxy :: Proxy EditChoiceEvent)
  , deleteEventSchema "DeleteChoiceEvent"
  , eventSchema "AddExpertEvent" (Proxy :: Proxy AddExpertEvent)
  , eventSchema "EditExpertEvent" (Proxy :: Proxy EditExpertEvent)
  , deleteEventSchema "DeleteExpertEvent"
  , eventSchema "AddReferenceEvent" (Proxy :: Proxy AddReferenceEvent)
  , eventSchema "EditReferenceEvent" (Proxy :: Proxy EditReferenceEvent)
  , deleteEventSchema "DeleteReferenceEvent"
  , eventSchema "AddTagEvent" (Proxy :: Proxy AddTagEvent)
  , eventSchema "EditTagEvent" (Proxy :: Proxy EditTagEvent)
  , deleteEventSchema "DeleteTagEvent"
  , eventSchema "AddIntegrationEvent" (Proxy :: Proxy AddIntegrationEvent)
  , eventSchema "EditIntegrationEvent" (Proxy :: Proxy EditIntegrationEvent)
  , deleteEventSchema "DeleteIntegrationEvent"
  , eventSchema "AddMetricEvent" (Proxy :: Proxy AddMetricEvent)
  , eventSchema "EditMetricEvent" (Proxy :: Proxy EditMetricEvent)
  , deleteEventSchema "DeleteMetricEvent"
  , eventSchema "AddPhaseEvent" (Proxy :: Proxy AddPhaseEvent)
  , eventSchema "EditPhaseEvent" (Proxy :: Proxy EditPhaseEvent)
  , deleteEventSchema "DeletePhaseEvent"
  , eventSchema "AddResourceCollectionEvent" (Proxy :: Proxy AddResourceCollectionEvent)
  , eventSchema "EditResourceCollectionEvent" (Proxy :: Proxy EditResourceCollectionEvent)
  , deleteEventSchema "DeleteResourceCollectionEvent"
  , eventSchema "AddResourcePageEvent" (Proxy :: Proxy AddResourcePageEvent)
  , eventSchema "EditResourcePageEvent" (Proxy :: Proxy EditResourcePageEvent)
  , deleteEventSchema "DeleteResourcePageEvent"
  , eventSchema "MoveQuestionEvent" (Proxy :: Proxy MoveQuestionEvent)
  , eventSchema "MoveAnswerEvent" (Proxy :: Proxy MoveAnswerEvent)
  , eventSchema "MoveChoiceEvent" (Proxy :: Proxy MoveChoiceEvent)
  , eventSchema "MoveExpertEvent" (Proxy :: Proxy MoveExpertEvent)
  , eventSchema "MoveReferenceEvent" (Proxy :: Proxy MoveReferenceEvent)
  ]

eventSchema :: JsonSchema a => String -> Proxy a -> (String, Value)
eventSchema eventType proxy = (eventType, withConstProperties [("eventType", eventType)] (jsonSchema proxy))

deleteEventSchema :: String -> (String, Value)
deleteEventSchema eventType = (eventType, withConstProperties [("eventType", eventType)] (objectSchema [] []))
