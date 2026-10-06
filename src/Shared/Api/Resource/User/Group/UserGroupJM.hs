module Shared.Api.Resource.User.Group.UserGroupJM where

import Data.Aeson

import Shared.Model.User.UserGroup
import Shared.Util.Aeson

instance FromJSON UserGroup where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroup where
  toJSON = genericToJSON jsonOptions
