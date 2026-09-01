module WizardServer.Api.Resource.User.UserStateJM where

import Data.Aeson

import Shared.Api.Resource.User.UserStateDTO
import Shared.Util.Aeson

instance FromJSON UserStateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserStateDTO where
  toJSON = genericToJSON jsonOptions
