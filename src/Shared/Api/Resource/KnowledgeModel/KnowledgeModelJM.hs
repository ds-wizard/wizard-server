module Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM where

import Control.Monad
import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Aeson
import Shared.Util.JsonSchema

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON KnowledgeModel where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModel where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEntities where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelEntities where
  parseJSON = genericParseJSON jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Chapter where
  toJSON = genericToJSON jsonOptions

instance FromJSON Chapter where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema Chapter where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON QuestionValueType

instance FromJSON QuestionValueType

instance JsonSchema QuestionValueType where
  jsonSchema = genericJsonSchema defaultOptions

instance ToJSON Question where
  toJSON = toSumJSON

instance FromJSON Question where
  parseJSON (Object o) = do
    questionType <- o .: "questionType"
    case questionType of
      "OptionsQuestion" -> parseJSON (Object o) >>= \event -> return (OptionsQuestion' event)
      "MultiChoiceQuestion" -> parseJSON (Object o) >>= \event -> return (MultiChoiceQuestion' event)
      "ListQuestion" -> parseJSON (Object o) >>= \event -> return (ListQuestion' event)
      "ValueQuestion" -> parseJSON (Object o) >>= \event -> return (ValueQuestion' event)
      "IntegrationQuestion" -> parseJSON (Object o) >>= \event -> return (IntegrationQuestion' event)
      "ItemSelectQuestion" -> parseJSON (Object o) >>= \event -> return (ItemSelectQuestion' event)
      "FileQuestion" -> parseJSON (Object o) >>= \event -> return (FileQuestion' event)
      _ -> fail "One of the questions has unsupported questionType"
  parseJSON _ = mzero

instance JsonSchema Question where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy OptionsQuestion)
      , jsonSchema (Proxy :: Proxy MultiChoiceQuestion)
      , jsonSchema (Proxy :: Proxy ListQuestion)
      , jsonSchema (Proxy :: Proxy ValueQuestion)
      , jsonSchema (Proxy :: Proxy IntegrationQuestion)
      , jsonSchema (Proxy :: Proxy ItemSelectQuestion)
      , jsonSchema (Proxy :: Proxy FileQuestion)
      ]

-- --------------------------------------------------------------------
instance ToJSON OptionsQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON OptionsQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

instance JsonSchema OptionsQuestion where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON MultiChoiceQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON MultiChoiceQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

instance JsonSchema MultiChoiceQuestion where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON ListQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON ListQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

instance JsonSchema ListQuestion where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON ValueQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON ValueQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

instance JsonSchema ValueQuestion where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "questionType")

instance ToJSON QuestionValidation where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON QuestionValidation where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance JsonSchema QuestionValidation where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "type")

-- --------------------------------------------------------------------
instance ToJSON IntegrationQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON IntegrationQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

instance JsonSchema IntegrationQuestion where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON ItemSelectQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON ItemSelectQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

instance JsonSchema ItemSelectQuestion where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
instance ToJSON FileQuestion where
  toJSON = genericToJSON (jsonOptionsWithTypeField "questionType")

instance FromJSON FileQuestion where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "questionType")

instance JsonSchema FileQuestion where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "questionType")

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Answer where
  toJSON = genericToJSON jsonOptions

instance FromJSON Answer where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema Answer where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Choice where
  toJSON = genericToJSON jsonOptions

instance FromJSON Choice where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema Choice where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Expert where
  toJSON = genericToJSON jsonOptions

instance FromJSON Expert where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema Expert where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Reference where
  toJSON = toSumJSON

instance FromJSON Reference where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (ResourcePageReference' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (URLReference' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (CrossReference' event)
      _ -> fail "One of the references has unsupported referenceType"
  parseJSON _ = mzero

instance JsonSchema Reference where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy ResourcePageReference)
      , jsonSchema (Proxy :: Proxy URLReference)
      , jsonSchema (Proxy :: Proxy CrossReference)
      ]

-- --------------------------------------------------------------------
instance ToJSON ResourcePageReference where
  toJSON = genericToJSON (jsonOptionsWithTypeField "referenceType")

instance FromJSON ResourcePageReference where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "referenceType")

instance JsonSchema ResourcePageReference where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "referenceType")

-- --------------------------------------------------------------------
instance ToJSON URLReference where
  toJSON = genericToJSON (jsonOptionsWithTypeField "referenceType")

instance FromJSON URLReference where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "referenceType")

instance JsonSchema URLReference where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "referenceType")

-- --------------------------------------------------------------------
instance ToJSON CrossReference where
  toJSON = genericToJSON (jsonOptionsWithTypeField "referenceType")

instance FromJSON CrossReference where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "referenceType")

instance JsonSchema CrossReference where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "referenceType")

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Metric where
  toJSON = genericToJSON jsonOptions

instance FromJSON Metric where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema Metric where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
instance ToJSON MetricMeasure where
  toJSON = genericToJSON jsonOptions

instance FromJSON MetricMeasure where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema MetricMeasure where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Phase where
  toJSON = genericToJSON jsonOptions

instance FromJSON Phase where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema Phase where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Tag where
  toJSON = genericToJSON jsonOptions

instance FromJSON Tag where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema Tag where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON ResourceCollection where
  toJSON = genericToJSON jsonOptions

instance FromJSON ResourceCollection where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema ResourceCollection where
  jsonSchema = genericJsonSchema jsonOptions

instance ToJSON ResourcePage where
  toJSON = genericToJSON jsonOptions

instance FromJSON ResourcePage where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema ResourcePage where
  jsonSchema = genericJsonSchema jsonOptions

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
instance ToJSON Integration where
  toJSON = toSumJSON

instance FromJSON Integration where
  parseJSON (Object o) = do
    referenceType <- o .: "integrationType"
    case referenceType of
      "ApiIntegration" -> parseJSON (Object o) >>= \event -> return (ApiIntegration' event)
      "PluginIntegration" -> parseJSON (Object o) >>= \event -> return (PluginIntegration' event)
      _ -> fail "One of the integrations has unsupported integrationType"
  parseJSON _ = mzero

instance JsonSchema Integration where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy ApiIntegration)
      , jsonSchema (Proxy :: Proxy PluginIntegration)
      ]

-- --------------------------------------------------------------------
instance ToJSON ApiIntegration where
  toJSON = genericToJSON (jsonOptionsWithTypeField "integrationType")

instance FromJSON ApiIntegration where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "integrationType")

instance JsonSchema ApiIntegration where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "integrationType")

instance ToJSON TypeHintExchange where
  toJSON = genericToJSON jsonOptions

instance FromJSON TypeHintExchange where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema TypeHintExchange where
  jsonSchema = genericJsonSchema jsonOptions

instance ToJSON TypeHintRequest where
  toJSON = genericToJSON jsonOptions

instance FromJSON TypeHintRequest where
  parseJSON = genericParseJSON jsonOptions

instance JsonSchema TypeHintRequest where
  jsonSchema = genericJsonSchema jsonOptions

instance ToJSON TypeHintResponse where
  toJSON = toSumJSON

instance FromJSON TypeHintResponse where
  parseJSON (Object o) = do
    responseType <- o .: "responseType"
    case responseType of
      "SuccessTypeHintResponse" -> parseJSON (Object o) >>= \event -> return (SuccessTypeHintResponse' event)
      "RemoteErrorTypeHintResponse" -> parseJSON (Object o) >>= \event -> return (RemoteErrorTypeHintResponse' event)
      "RequestFailedTypeHintResponse" -> parseJSON (Object o) >>= \event -> return (RequestFailedTypeHintResponse' event)
      _ -> fail "One of the integrations has unsupported responseType"
  parseJSON _ = mzero

instance JsonSchema TypeHintResponse where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy SuccessTypeHintResponse)
      , jsonSchema (Proxy :: Proxy RemoteErrorTypeHintResponse)
      , jsonSchema (Proxy :: Proxy RequestFailedTypeHintResponse)
      ]

instance ToJSON SuccessTypeHintResponse where
  toJSON = genericToJSON (jsonOptionsWithTypeField "responseType")

instance FromJSON SuccessTypeHintResponse where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "responseType")

instance JsonSchema SuccessTypeHintResponse where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "responseType")

instance ToJSON RemoteErrorTypeHintResponse where
  toJSON = genericToJSON (jsonOptionsWithTypeField "responseType")

instance FromJSON RemoteErrorTypeHintResponse where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "responseType")

instance JsonSchema RemoteErrorTypeHintResponse where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "responseType")

instance ToJSON RequestFailedTypeHintResponse where
  toJSON = genericToJSON (jsonOptionsWithTypeField "responseType")

instance FromJSON RequestFailedTypeHintResponse where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "responseType")

instance JsonSchema RequestFailedTypeHintResponse where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "responseType")

-- --------------------------------------------------------------------
instance ToJSON PluginIntegration where
  toJSON = genericToJSON (jsonOptionsWithTypeField "integrationType")

instance FromJSON PluginIntegration where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "integrationType")

instance JsonSchema PluginIntegration where
  jsonSchema = genericJsonSchema (jsonOptionsWithTypeField "integrationType")
