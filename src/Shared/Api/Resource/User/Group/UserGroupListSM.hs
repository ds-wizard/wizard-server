module Shared.Api.Resource.User.Group.UserGroupListSM where

import Data.Swagger

import Shared.Api.Resource.User.Group.UserGroupListJM ()
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.User.UserGroupList
import Shared.Service.User.Group.UserGroupMapper
import Shared.Util.Swagger

instance ToSchema UserGroupList where
  declareNamedSchema = toSwagger (toList bioGroup True True)
