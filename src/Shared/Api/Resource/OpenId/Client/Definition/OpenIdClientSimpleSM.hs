module Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleSM where

import Data.Swagger

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM ()
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import Shared.Model.OpenId.OpenIdClientSimple
import Shared.Util.Swagger

instance ToSchema OpenIdClientSimple where
  declareNamedSchema = toSwagger defaultOpenIdClientSimple
