module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM where

import Data.Aeson

import Shared.Model.OpenId.OpenIdClientStyle
import Shared.Util.Aeson

instance FromJSON OpenIdClientStyle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientStyle where
  toJSON = genericToJSON jsonOptions
