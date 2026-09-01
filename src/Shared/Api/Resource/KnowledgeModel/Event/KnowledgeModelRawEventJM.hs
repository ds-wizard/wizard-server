module Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelRawEventJM where

import Data.Aeson

import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Model.KnowledgeModel.Event.KnowledgeModelRawEvent
import Shared.Util.Aeson

instance ToJSON KnowledgeModelRawEvent where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelRawEvent where
  parseJSON = genericParseJSON jsonOptions
