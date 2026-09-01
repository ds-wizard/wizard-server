module Shared.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldJM ()
import Shared.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Shared.Util.Aeson

instance FromJSON AddResourceCollectionEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddResourceCollectionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditResourceCollectionEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditResourceCollectionEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON DeleteResourceCollectionEvent where
  parseJSON _ = pure DeleteResourceCollectionEvent

instance ToJSON DeleteResourceCollectionEvent where
  toJSON _ = toJSON [("eventType", String "DeleteResourceCollectionEvent")]

-- --------------------------------------------
-- --------------------------------------------
instance FromJSON AddResourcePageEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON AddResourcePageEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")

-- --------------------------------------------
instance FromJSON EditResourcePageEvent where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "eventType")

instance ToJSON EditResourcePageEvent where
  toJSON = genericToJSON (jsonOptionsWithTypeField "eventType")
