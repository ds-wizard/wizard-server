module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterSM where

import Data.Swagger

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.Database.Migration.Development.OpenId.Data.OpenIds
import Shared.Model.OpenId.OpenIdClientParameter
import Shared.Util.Swagger

instance ToSchema OpenIdClientParameter where
  declareNamedSchema = toSwagger openIdClientDefinitionParameter
