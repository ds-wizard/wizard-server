module Shared.Api.Resource.User.Group.UserGroupDetailSM where

import Data.Swagger

import Shared.Api.Resource.User.Group.UserGroupDetailDTO
import Shared.Api.Resource.User.Group.UserGroupDetailJM ()
import Shared.Api.Resource.User.UserWithMembershipSM ()
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Service.User.Group.UserGroupMapper
import Shared.Util.Swagger

instance ToSchema UserGroupDetailDTO where
  declareNamedSchema = toSwagger (toDetailDTO bioGroup [])
