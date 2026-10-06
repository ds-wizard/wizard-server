module Shared.Api.Resource.User.Group.UserGroupChangeSM where

import Data.Swagger

import Shared.Api.Resource.User.Group.UserGroupChangeDTO
import Shared.Api.Resource.User.Group.UserGroupChangeJM ()
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Service.User.Group.UserGroupMapper
import Shared.Util.Swagger

instance ToSchema UserGroupChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO bioGroup)
