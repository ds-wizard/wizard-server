module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM where

import Data.Aeson

import Shared.Model.OpenId.OpenIdClientParameter
import Shared.Util.Aeson

instance FromJSON OpenIdClientParameter where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientParameter where
  toJSON = genericToJSON jsonOptions
