module Shared.Api.Resource.UserToken.ApiKeyCreateSM where

import Data.Swagger

import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Api.Resource.UserToken.ApiKeyCreateJM ()
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Util.Swagger

instance ToSchema ApiKeyCreateDTO where
  declareNamedSchema = toSwagger albertCreateApiKey
