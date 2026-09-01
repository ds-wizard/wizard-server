module Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorWebSocketEventJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesJM ()
import Shared.Api.Resource.User.OnlineUserInfoJM ()
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Util.Aeson

instance FromJSON ClientKnowledgeModelEditorMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientKnowledgeModelEditorMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ServerKnowledgeModelEditorMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ServerKnowledgeModelEditorMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
