module WizardServer.Api.Resource.User.UserProfileChangeJM where

import Data.Aeson

import Shared.Api.Resource.User.UserProfileChangeDTO
import Shared.Util.Aeson

instance FromJSON UserProfileChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserProfileChangeDTO where
  toJSON = genericToJSON jsonOptions
