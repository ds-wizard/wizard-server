module Shared.Api.Resource.User.Group.UserGroupChangeJM where

import Data.Aeson

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON UserGroupChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupChangeDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema UserGroupChangeDTO where
  jsonSchema = genericJsonSchema jsonOptions
