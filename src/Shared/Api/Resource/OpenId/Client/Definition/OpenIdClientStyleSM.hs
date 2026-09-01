module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM where

import Data.Swagger

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.Database.Migration.Development.OpenId.Data.OpenIds
import Shared.Model.OpenId.OpenIdClientStyle
import Shared.Util.Swagger

instance ToSchema OpenIdClientStyle where
  declareNamedSchema = toSwagger openIdClientDefinitionStyle
