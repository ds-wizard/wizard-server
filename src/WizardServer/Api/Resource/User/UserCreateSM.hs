module WizardServer.Api.Resource.User.UserCreateSM where

import Data.Swagger

import Shared.Api.Resource.User.UserCreateDTO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.UserCreateJM ()

instance ToSchema UserCreateDTO where
  declareNamedSchema = toSwagger userJohnCreate
