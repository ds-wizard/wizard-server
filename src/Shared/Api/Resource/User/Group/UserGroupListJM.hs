module Shared.Api.Resource.User.Group.UserGroupListJM where

import Data.Aeson

import Shared.Model.User.UserGroupList
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON UserGroupList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupList where
  toJSON = genericToJSON jsonOptions

instance JsonSchema UserGroupList where
  jsonSchema = genericJsonSchema jsonOptions
