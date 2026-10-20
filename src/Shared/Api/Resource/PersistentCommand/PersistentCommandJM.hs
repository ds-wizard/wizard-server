module Shared.Api.Resource.PersistentCommand.PersistentCommandJM where

import Data.Aeson

import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON PersistentCommandState

instance ToJSON PersistentCommandState

instance FromJSON identity => FromJSON (PersistentCommand identity) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON identity => ToJSON (PersistentCommand identity) where
  toJSON = genericToJSON jsonOptions

instance JsonSchema PersistentCommandState where
  jsonSchema = genericJsonSchema defaultOptions
