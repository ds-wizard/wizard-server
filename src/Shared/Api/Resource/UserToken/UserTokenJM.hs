module Shared.Api.Resource.UserToken.UserTokenJM where

import Data.Aeson

import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Model.User.UserToken
import Shared.Util.Aeson

instance ToJSON UserTokenType

instance ToJSON UserToken where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserTokenDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON UserTokenDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
