module WizardServer.Api.Resource.User.UserSubmissionPropListSM where

import Data.Swagger

import Shared.Api.Resource.User.UserSubmissionPropListJM ()
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.UserSubmissionPropList
import Shared.Util.Swagger

instance ToSchema UserSubmissionPropList where
  declareNamedSchema = toSwagger userAlbertApiTokenList
