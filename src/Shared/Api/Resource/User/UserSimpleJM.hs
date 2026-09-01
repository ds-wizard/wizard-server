module Shared.Api.Resource.User.UserSimpleJM where

import Data.Aeson

import Shared.Model.User.UserSimple
import Shared.Util.Aeson

instance FromJSON UserSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSimple where
  toJSON = genericToJSON jsonOptions
