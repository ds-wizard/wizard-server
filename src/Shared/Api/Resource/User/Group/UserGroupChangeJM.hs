module Shared.Api.Resource.User.Group.UserGroupChangeJM where

import Data.Aeson

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Util.Aeson

instance FromJSON UserGroupChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupChangeDTO where
  toJSON = genericToJSON jsonOptions
