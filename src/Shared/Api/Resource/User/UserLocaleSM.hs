module Shared.Api.Resource.User.UserLocaleSM where

import Data.Swagger

import Shared.Api.Resource.User.UserLocaleDTO
import Shared.Api.Resource.User.UserLocaleJM ()
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Util.Swagger

instance ToSchema UserLocaleDTO where
  declareNamedSchema = toSwagger userLocale
