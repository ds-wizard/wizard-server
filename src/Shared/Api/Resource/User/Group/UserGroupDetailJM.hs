module Shared.Api.Resource.User.Group.UserGroupDetailJM where

import Data.Aeson

import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Api.Resource.User.UserWithMembershipJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON UserGroupDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupDetailDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema UserGroupDetailDTO where
  jsonSchema = genericJsonSchema jsonOptions
