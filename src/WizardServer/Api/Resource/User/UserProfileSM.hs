module WizardServer.Api.Resource.User.UserProfileSM where

import Data.Swagger

import Shared.Api.Resource.Common.AesonSM ()
import Shared.Api.Resource.User.RoleSimpleSM ()
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.UserProfile
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.UserProfileJM ()

instance ToSchema UserProfile where
  declareNamedSchema = toSwagger userAlbertProfile
