module Shared.Api.Resource.Auth.AuthConsentJM where

import Data.Aeson

import Shared.Api.Resource.Auth.AuthConsentDTO
import Shared.Util.Aeson

instance FromJSON AuthConsentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AuthConsentDTO where
  toJSON = genericToJSON jsonOptions
