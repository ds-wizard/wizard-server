module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM where

import Data.Aeson

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.Util.Aeson

instance FromJSON OpenIdClientDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientDetailDTO where
  toJSON = genericToJSON jsonOptions
