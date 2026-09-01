module Shared.Api.Resource.User.OnlineUserInfoJM where

import Data.Aeson
import Shared.Api.Resource.User.RoleSimpleJM ()

import Shared.Model.User.OnlineUserInfo
import Shared.Util.Aeson

instance FromJSON OnlineUserInfo where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON OnlineUserInfo where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
