module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM where

import Data.Aeson

import Shared.Model.OpenId.OpenIdClientStyle
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON OpenIdClientStyle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientStyle where
  toJSON = genericToJSON jsonOptions

instance JsonSchema OpenIdClientStyle where
  jsonSchema = genericJsonSchema jsonOptions
