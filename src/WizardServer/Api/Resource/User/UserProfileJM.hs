module WizardServer.Api.Resource.User.UserProfileJM where

import Data.Aeson

import Shared.Api.Resource.User.RoleSimpleJM ()
import Shared.Model.User.UserProfile
import Shared.Util.Aeson

instance FromJSON UserProfile where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserProfile where
  toJSON = genericToJSON jsonOptions
