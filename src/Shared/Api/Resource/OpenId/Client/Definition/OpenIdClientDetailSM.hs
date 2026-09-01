module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailSM where

import Data.Swagger

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterSM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import Shared.Util.Swagger

instance ToSchema OpenIdClientDetailDTO where
  declareNamedSchema = toSwagger defaultOpenIdClientDetailDto
