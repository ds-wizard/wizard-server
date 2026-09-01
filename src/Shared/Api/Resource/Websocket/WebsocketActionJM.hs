module Shared.Api.Resource.Websocket.WebsocketActionJM where

import Data.Aeson

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Util.Aeson

instance ToJSON a => ToJSON (Success_ServerActionDTO a) where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON a => FromJSON (Success_ServerActionDTO a) where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON Error_ServerActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON Error_ServerActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")
