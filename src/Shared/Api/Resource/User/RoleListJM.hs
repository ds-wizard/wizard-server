module Shared.Api.Resource.User.RoleListJM where

import Data.Aeson

import Shared.Model.User.RoleList
import Shared.Util.Aeson

instance FromJSON RoleList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RoleList where
  toJSON = genericToJSON jsonOptions
