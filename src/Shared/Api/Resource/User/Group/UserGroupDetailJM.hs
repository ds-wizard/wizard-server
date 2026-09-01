module Shared.Api.Resource.User.Group.UserGroupDetailJM where

import Data.Aeson

import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Api.Resource.User.UserWithMembershipJM ()
import Shared.Util.Aeson

instance FromJSON UserGroupDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupDetailDTO where
  toJSON = genericToJSON jsonOptions
