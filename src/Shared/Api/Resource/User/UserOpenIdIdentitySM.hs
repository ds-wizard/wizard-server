module Shared.Api.Resource.User.UserOpenIdIdentitySM where

import Data.Swagger

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import Shared.Api.Resource.User.UserOpenIdIdentityDTO
import Shared.Api.Resource.User.UserOpenIdIdentityJM ()
import Shared.Database.Migration.Development.User.Data.UserOpenIdIdentities
import Shared.Util.Swagger

instance ToSchema UserOpenIdIdentityDTO where
  declareNamedSchema = toSwagger defaultUserOpenIdIdentityDto
