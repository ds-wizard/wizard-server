module WizardServer.Api.Resource.User.UserChangeJM where

import Data.Aeson

import Shared.Api.Resource.User.UserChangeDTO
import Shared.Util.Aeson

instance FromJSON UserChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserChangeDTO where
  toJSON = genericToJSON jsonOptions
