module Shared.Api.Resource.User.Group.UserGroupSM where

import Data.Swagger

import Shared.Api.Resource.User.Group.UserGroupJM ()
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.User.UserGroup
import Shared.Util.Swagger

instance ToSchema UserGroup where
  declareNamedSchema = toSwagger bioGroup
