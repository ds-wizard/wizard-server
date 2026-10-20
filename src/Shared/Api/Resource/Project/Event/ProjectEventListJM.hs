module Shared.Api.Resource.Project.Event.ProjectEventListJM where

import Control.Monad
import Data.Aeson
import Data.Proxy

import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Project.Event.ProjectEventList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON ProjectEventList where
  toJSON = toSumJSONWithTypeField "type" "List"

instance FromJSON ProjectEventList where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "SetReplyEvent" -> parseJSON (Object o) >>= \event -> return (SetReplyEventList' event)
      "ClearReplyEvent" -> parseJSON (Object o) >>= \event -> return (ClearReplyEventList' event)
      "SetPhaseEvent" -> parseJSON (Object o) >>= \event -> return (SetPhaseEventList' event)
      "SetLabelsEvent" -> parseJSON (Object o) >>= \event -> return (SetLabelsEventList' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON SetReplyEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetReplyEventList where
  toJSON = genericToJSON jsonOptions

instance FromJSON ClearReplyEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ClearReplyEventList where
  toJSON = genericToJSON jsonOptions

instance FromJSON SetPhaseEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetPhaseEventList where
  toJSON = genericToJSON jsonOptions

instance FromJSON SetLabelsEventList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetLabelsEventList where
  toJSON = genericToJSON jsonOptions

instance JsonSchema ProjectEventList where
  jsonSchema _ =
    anyOfSchema
      [ eventSchema "SetReplyEvent" (Proxy :: Proxy SetReplyEventList)
      , eventSchema "ClearReplyEvent" (Proxy :: Proxy ClearReplyEventList)
      , eventSchema "SetPhaseEvent" (Proxy :: Proxy SetPhaseEventList)
      , eventSchema "SetLabelsEvent" (Proxy :: Proxy SetLabelsEventList)
      ]

eventSchema :: JsonSchema a => String -> Proxy a -> Value
eventSchema eventType proxy = withConstProperties [("type", eventType)] (jsonSchema proxy)

instance JsonSchema SetReplyEventList where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema ClearReplyEventList where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema SetPhaseEventList where
  jsonSchema = genericJsonSchema jsonOptions

instance JsonSchema SetLabelsEventList where
  jsonSchema = genericJsonSchema jsonOptions
