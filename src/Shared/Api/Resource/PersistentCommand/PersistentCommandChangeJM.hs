module Shared.Api.Resource.PersistentCommand.PersistentCommandChangeJM where

import Data.Aeson

import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.Util.Aeson

instance FromJSON PersistentCommandChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandChangeDTO where
  toJSON = genericToJSON jsonOptions
