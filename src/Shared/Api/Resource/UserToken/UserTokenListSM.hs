module Shared.Api.Resource.UserToken.UserTokenListSM where

import Data.Swagger

import Shared.Api.Resource.UserToken.UserTokenListJM ()
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Model.User.UserTokenList
import Shared.Service.UserToken.UserTokenMapper
import Shared.Util.Swagger

instance ToSchema UserTokenList where
  declareNamedSchema = toSwagger (toList albertToken True)
