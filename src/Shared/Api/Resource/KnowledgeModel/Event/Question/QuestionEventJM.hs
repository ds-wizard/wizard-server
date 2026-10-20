module Shared.Api.Resource.KnowledgeModel.Event.Question.QuestionEventJM where

import Control.Monad
import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON AddQuestionEvent where
  toJSON = toSumJSON

instance JsonSchema AddQuestionEvent where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy AddOptionsQuestionEvent)
      , jsonSchema (Proxy :: Proxy AddMultiChoiceQuestionEvent)
      , jsonSchema (Proxy :: Proxy AddListQuestionEvent)
      , jsonSchema (Proxy :: Proxy AddValueQuestionEvent)
      , jsonSchema (Proxy :: Proxy AddIntegrationQuestionEvent)
      , jsonSchema (Proxy :: Proxy AddItemSelectQuestionEvent)
      , jsonSchema (Proxy :: Proxy AddFileQuestionEvent)
      ]

instance FromJSON AddQuestionEvent where
  parseJSON (Object o) = do
    referenceType <- o .: "questionType"
    case referenceType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (AddOptionsQuestionEvent' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (AddMultiChoiceQuestionEvent' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (AddListQuestionEvent' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (AddValueQuestionEvent' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (AddIntegrationQuestionEvent' event)
      "ItemSelectQuestion" -> parseJSON (Object o) >>= \event -> return (AddItemSelectQuestionEvent' event)
      "FileQuestion" -> parseJSON (Object o) >>= \event -> return (AddFileQuestionEvent' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddOptionsQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddOptionsQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "OptionsQuestion")]

instance JsonSchema AddOptionsQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "OptionsQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddMultiChoiceQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddMultiChoiceQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "MultiChoiceQuestion")]

instance JsonSchema AddMultiChoiceQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "MultiChoiceQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddListQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddListQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ListQuestion")]

instance JsonSchema AddListQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "ListQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddValueQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddValueQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ValueQuestion")]

instance JsonSchema AddValueQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "ValueQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddIntegrationQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddIntegrationQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "IntegrationQuestion")]

instance JsonSchema AddIntegrationQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "IntegrationQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddItemSelectQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddItemSelectQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ItemSelectQuestion")]

instance JsonSchema AddItemSelectQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "ItemSelectQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddFileQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddFileQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "FileQuestion")]

instance JsonSchema AddFileQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "FileQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditQuestionEvent where
  toJSON = toSumJSON

instance JsonSchema EditQuestionEvent where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy EditOptionsQuestionEvent)
      , jsonSchema (Proxy :: Proxy EditMultiChoiceQuestionEvent)
      , jsonSchema (Proxy :: Proxy EditListQuestionEvent)
      , jsonSchema (Proxy :: Proxy EditValueQuestionEvent)
      , jsonSchema (Proxy :: Proxy EditIntegrationQuestionEvent)
      , jsonSchema (Proxy :: Proxy EditItemSelectQuestionEvent)
      , jsonSchema (Proxy :: Proxy EditFileQuestionEvent)
      ]

instance FromJSON EditQuestionEvent where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (EditOptionsQuestionEvent' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (EditMultiChoiceQuestionEvent' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (EditListQuestionEvent' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (EditValueQuestionEvent' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (EditIntegrationQuestionEvent' event)
      "ItemSelectQuestion" -> parseJSON (Object o) >>= \event -> return (EditItemSelectQuestionEvent' event)
      "FileQuestion" -> parseJSON (Object o) >>= \event -> return (EditFileQuestionEvent' event)
      _ -> fail "One of the events has unsupported questionType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditOptionsQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditOptionsQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "OptionsQuestion")]

instance JsonSchema EditOptionsQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "OptionsQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditMultiChoiceQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditMultiChoiceQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "MultiChoiceQuestion")]

instance JsonSchema EditMultiChoiceQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "MultiChoiceQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditListQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditListQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ListQuestion")]

instance JsonSchema EditListQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "ListQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditValueQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditValueQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ValueQuestion")]

instance JsonSchema EditValueQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "ValueQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditIntegrationQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditIntegrationQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "IntegrationQuestion")]

instance JsonSchema EditIntegrationQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "IntegrationQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditItemSelectQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditItemSelectQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "ItemSelectQuestion")]

instance JsonSchema EditItemSelectQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "ItemSelectQuestion")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditFileQuestionEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditFileQuestionEvent where
  toJSON = toJSONWithAdditionalData [("questionType", "FileQuestion")]

instance JsonSchema EditFileQuestionEvent where
  jsonSchema = withConstProperties [("questionType", "FileQuestion")] . genericJsonSchema jsonOptions
