module Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventJM where

import Control.Monad
import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventDTO
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Util.Aeson

instance ToJSON KnowledgeModelEditorWebSocketEventDTO where
  toJSON = toSumJSON

instance FromJSON KnowledgeModelEditorWebSocketEventDTO where
  parseJSON (Object o) = do
    eventType <- o .: "type"
    case eventType of
      "AddKnowledgeModelEditorWebSocketEvent" -> parseJSON (Object o) >>= \event -> return (AddKnowledgeModelEditorWebSocketEventDTO' event)
      _ -> fail "One of the events has unsupported type"
  parseJSON _ = mzero

instance FromJSON AddKnowledgeModelEditorWebSocketEventDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AddKnowledgeModelEditorWebSocketEventDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
