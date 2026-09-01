module WizardServer.Api.Resource.User.UserStateSM where

import Data.Swagger

import Shared.Api.Resource.User.UserStateDTO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.UserStateJM ()

instance ToSchema UserStateDTO where
  declareNamedSchema = toSwagger userState
