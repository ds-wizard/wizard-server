module Shared.Api.Resource.User.UserSM where

import Data.Swagger

import Shared.Api.Resource.User.RoleSimpleSM ()
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Api.Resource.User.UserSubmissionPropSM ()
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Service.User.WizardUserMapper
import Shared.Util.Swagger

instance ToSchema UserDTO where
  declareNamedSchema = toSwagger (toDTO userAlbert)
