module Shared.Api.Resource.UserEmailLink.UserEmailLinkJM where

import Data.Aeson

import Shared.Api.Resource.UserEmailLink.UserEmailLinkDTO
import Shared.Util.Aeson

instance FromJSON aType => FromJSON (UserEmailLinkDTO aType) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON aType => ToJSON (UserEmailLinkDTO aType) where
  toJSON = genericToJSON jsonOptions
