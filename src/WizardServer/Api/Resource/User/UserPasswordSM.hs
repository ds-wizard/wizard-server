module WizardServer.Api.Resource.User.UserPasswordSM where

import Data.Swagger

import Shared.Api.Resource.User.UserPasswordDTO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.UserPasswordJM ()

instance ToSchema UserPasswordDTO where
  declareNamedSchema = toSwagger userPassword
