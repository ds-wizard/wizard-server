module Shared.Api.Resource.KnowledgeModel.Event.Reference.ReferenceEventJM where

import Control.Monad
import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.Reference.ReferenceEvent
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON AddReferenceEvent where
  toJSON = toSumJSON

instance JsonSchema AddReferenceEvent where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy AddResourcePageReferenceEvent)
      , jsonSchema (Proxy :: Proxy AddURLReferenceEvent)
      , jsonSchema (Proxy :: Proxy AddCrossReferenceEvent)
      ]

instance FromJSON AddReferenceEvent where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (AddResourcePageReferenceEvent' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (AddURLReferenceEvent' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (AddCrossReferenceEvent' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON AddResourcePageReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddResourcePageReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "ResourcePageReference")]

instance JsonSchema AddResourcePageReferenceEvent where
  jsonSchema = withConstProperties [("referenceType", "ResourcePageReference")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddURLReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddURLReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "URLReference")]

instance JsonSchema AddURLReferenceEvent where
  jsonSchema = withConstProperties [("referenceType", "URLReference")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON AddCrossReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddCrossReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "CrossReference")]

instance JsonSchema AddCrossReferenceEvent where
  jsonSchema = withConstProperties [("referenceType", "CrossReference")] . genericJsonSchema jsonOptions

-- --------------------------------------------
-- --------------------------------------------
instance ToJSON EditReferenceEvent where
  toJSON = toSumJSON

instance JsonSchema EditReferenceEvent where
  jsonSchema _ =
    oneOfSchema
      [ jsonSchema (Proxy :: Proxy EditResourcePageReferenceEvent)
      , jsonSchema (Proxy :: Proxy EditURLReferenceEvent)
      , jsonSchema (Proxy :: Proxy EditCrossReferenceEvent)
      ]

instance FromJSON EditReferenceEvent where
  parseJSON (Object o) = do
    referenceType <- o .: "referenceType"
    case referenceType of
      "ResourcePageReference" -> parseJSON (Object o) >>= \event -> return (EditResourcePageReferenceEvent' event)
      "URLReference" -> parseJSON (Object o) >>= \event -> return (EditURLReferenceEvent' event)
      "CrossReference" -> parseJSON (Object o) >>= \event -> return (EditCrossReferenceEvent' event)
      _ -> fail "One of the events has unsupported referenceType"
  parseJSON _ = mzero

-- --------------------------------------------
instance FromJSON EditResourcePageReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditResourcePageReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "ResourcePageReference")]

instance JsonSchema EditResourcePageReferenceEvent where
  jsonSchema = withConstProperties [("referenceType", "ResourcePageReference")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditURLReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditURLReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "URLReference")]

instance JsonSchema EditURLReferenceEvent where
  jsonSchema = withConstProperties [("referenceType", "URLReference")] . genericJsonSchema jsonOptions

-- --------------------------------------------
instance FromJSON EditCrossReferenceEvent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON EditCrossReferenceEvent where
  toJSON = toJSONWithAdditionalData [("referenceType", "CrossReference")]

instance JsonSchema EditCrossReferenceEvent where
  jsonSchema = withConstProperties [("referenceType", "CrossReference")] . genericJsonSchema jsonOptions
