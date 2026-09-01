module WizardServer.Api.Resource.User.UserProfileChangeSM where

import Data.Swagger

import Shared.Api.Resource.User.UserProfileChangeDTO
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.UserProfileChangeJM ()

instance ToSchema UserProfileChangeDTO where
  declareNamedSchema = toSwagger userIsaacProfileChange
