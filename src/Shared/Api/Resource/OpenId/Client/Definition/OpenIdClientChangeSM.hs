module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeSM where

import Data.Swagger

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterSM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import Shared.Util.Swagger

instance ToSchema OpenIdClientChangeDTO where
  declareNamedSchema = toSwagger defaultOpenIdClientChangeDto
