module Shared.Api.Resource.User.RoleSimpleSM where

import Data.Swagger

import Shared.Api.Resource.User.RoleSimpleJM ()
import Shared.Model.User.RoleSimple

instance ToSchema RoleSimple
