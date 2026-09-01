module Shared.Api.Resource.User.OnlineUserInfoSM where

import Data.Swagger

import Shared.Api.Resource.User.OnlineUserInfoJM ()
import Shared.Api.Resource.User.RoleSimpleSM ()
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.OnlineUserInfo
import Shared.Util.Swagger

instance ToSchema OnlineUserInfo where
  declareNamedSchema = toSwaggerWithType "type" userAlbertOnlineInfo
