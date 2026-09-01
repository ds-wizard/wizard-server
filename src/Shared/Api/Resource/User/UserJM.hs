module Shared.Api.Resource.User.UserJM where

import Data.Aeson
import Shared.Api.Resource.User.RoleSimpleJM ()

import Shared.Api.Resource.User.UserDTO
import Shared.Model.User.User
import Shared.Util.Aeson

instance ToJSON User where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserDTO where
  toJSON = genericToJSON jsonOptions
