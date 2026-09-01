module Shared.Api.Resource.PersistentCommand.PersistentCommandJM where

import Data.Aeson

import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Aeson

instance FromJSON PersistentCommandState

instance ToJSON PersistentCommandState

instance FromJSON identity => FromJSON (PersistentCommand identity) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON identity => ToJSON (PersistentCommand identity) where
  toJSON = genericToJSON jsonOptions
