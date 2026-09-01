module Shared.Api.Resource.User.UserSubmissionPropSM where

import Data.Swagger

import Shared.Api.Resource.User.UserSubmissionPropJM ()
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.UserSubmissionProp
import Shared.Util.Swagger

instance ToSchema UserSubmissionProp where
  declareNamedSchema = toSwagger userAlbertApiToken
