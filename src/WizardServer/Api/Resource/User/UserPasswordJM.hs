module WizardServer.Api.Resource.User.UserPasswordJM where

import Data.Aeson

import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Util.Aeson

instance FromJSON UserPasswordDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserPasswordDTO where
  toJSON = genericToJSON jsonOptions
