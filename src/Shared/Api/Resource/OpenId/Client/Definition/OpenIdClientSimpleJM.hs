module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM where

import Data.Aeson

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.Model.OpenId.OpenIdClientSimple
import Shared.Util.Aeson

instance FromJSON OpenIdClientSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientSimple where
  toJSON = genericToJSON jsonOptions
