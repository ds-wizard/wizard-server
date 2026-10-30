module Shared.Api.Resource.User.UserDetailSM where

import Data.Swagger

import Shared.Api.Resource.User.RoleSimpleSM ()
import Shared.Api.Resource.User.UserDetailDTO
import Shared.Api.Resource.User.UserDetailJM ()
import Shared.Api.Resource.User.UserOpenIdIdentitySM ()
import Shared.Database.Migration.Development.User.Data.UserOpenIdIdentities
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Service.User.WizardUserMapper
import Shared.Util.Swagger

instance ToSchema UserDetailDTO where
  declareNamedSchema = toSwagger (toUserDetailDTO userAlbert [defaultUserOpenIdIdentityDto])
