module Shared.Api.Resource.User.RoleListJM where

import Data.Aeson

import Shared.Model.User.RoleList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON RoleList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RoleList where
  toJSON = genericToJSON jsonOptions

instance JsonSchema RoleList where
  jsonSchema = genericJsonSchema jsonOptions
