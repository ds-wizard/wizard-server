module Shared.Api.Resource.UserToken.ApiKeyCreateJM where

import Data.Aeson

import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Util.Aeson

instance FromJSON ApiKeyCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ApiKeyCreateDTO where
  toJSON = genericToJSON jsonOptions
