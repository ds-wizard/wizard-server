module WizardServer.Api.Resource.User.UserSubmissionPropListSM where

import Data.Swagger

import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.UserSubmissionPropList
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.UserSubmissionPropListJM ()

instance ToSchema UserSubmissionPropList where
  declareNamedSchema = toSwagger userAlbertApiTokenList
