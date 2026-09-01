module Shared.Api.Resource.User.RoleChangeJM where

import Data.Aeson

import Shared.Api.Resource.User.RoleChangeDTO
import Shared.Util.Aeson

instance FromJSON RoleChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RoleChangeDTO where
  toJSON = genericToJSON jsonOptions
