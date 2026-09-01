module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM where

import Data.Aeson

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.Util.Aeson

instance FromJSON OpenIdClientChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientChangeDTO where
  toJSON = genericToJSON jsonOptions
