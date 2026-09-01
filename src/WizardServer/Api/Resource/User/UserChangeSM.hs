module WizardServer.Api.Resource.User.UserChangeSM where

import Data.Swagger

import Shared.Api.Resource.User.UserChangeDTO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.UserChangeJM ()

instance ToSchema UserChangeDTO where
  declareNamedSchema = toSwagger userIsaacEditedChange
