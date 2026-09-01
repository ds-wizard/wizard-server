module Shared.Api.Resource.User.UserLocaleJM where

import Data.Aeson

import Shared.Api.Resource.User.UserLocaleDTO
import Shared.Util.Aeson

instance FromJSON UserLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserLocaleDTO where
  toJSON = genericToJSON jsonOptions
