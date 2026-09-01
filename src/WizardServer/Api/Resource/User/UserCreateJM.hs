module WizardServer.Api.Resource.User.UserCreateJM where

import Data.Aeson

import Shared.Api.Resource.User.UserCreateDTO
import Shared.Util.Aeson

instance FromJSON UserCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserCreateDTO where
  toJSON = genericToJSON jsonOptions
