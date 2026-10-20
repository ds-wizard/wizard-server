module Shared.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventJM where

import Control.Monad
import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelJM ()
import Shared.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON AddIntegrationEvent where
  toJSON = toSumJSON

instance JsonSchema AddIntegrationEvent where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy AddApiIntegrationEvent)
      , jsonSchema (Proxy :: Proxy AddPluginIntegrationEvent)
      ]

instance FromJSON AddIntegrationEvent where
  parseJSON (Object o) = do
    integrationType <- o .: "integrationType"
    case integrationType of
      "ApiIntegration" -> parseJSON (Object o) >>= \event -> return (AddApiIntegrationEvent' event)
      "PluginIntegration" -> parseJSON (Object o) >>= \event -> return (AddPluginIntegrationEvent' event)
      _ -> fail "One of the events has unsupported integrationType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddApiIntegrationEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddApiIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "ApiIntegration")]

instance JsonSchema AddApiIntegrationEvent where
  jsonSchema = withConstProperties [("integrationType", "ApiIntegration")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddPluginIntegrationEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddPluginIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "PluginIntegration")]

instance JsonSchema AddPluginIntegrationEvent where
  jsonSchema = withConstProperties [("integrationType", "PluginIntegration")] . genericJsonSchema jsonOptions

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditIntegrationEvent where
  toJSON = toSumJSON

instance JsonSchema EditIntegrationEvent where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy EditApiIntegrationEvent)
      , jsonSchema (Proxy :: Proxy EditPluginIntegrationEvent)
      ]

instance FromJSON EditIntegrationEvent where
  parseJSON (Object o) = do
    integrationType <- o .: "integrationType"
    case integrationType of
      "ApiIntegration" -> parseJSON (Object o) >>= \event -> return (EditApiIntegrationEvent' event)
      "PluginIntegration" -> parseJSON (Object o) >>= \event -> return (EditPluginIntegrationEvent' event)
      _ -> fail "One of the events has unsupported integrationType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditApiIntegrationEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditApiIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "ApiIntegration")]

instance JsonSchema EditApiIntegrationEvent where
  jsonSchema = withConstProperties [("integrationType", "ApiIntegration")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditPluginIntegrationEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditPluginIntegrationEvent where
  toJSON = toJSONWithAdditionalData [("integrationType", "PluginIntegration")]

instance JsonSchema EditPluginIntegrationEvent where
  jsonSchema = withConstProperties [("integrationType", "PluginIntegration")] . genericJsonSchema jsonOptions
