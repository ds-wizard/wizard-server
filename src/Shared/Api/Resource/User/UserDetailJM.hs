module Shared.Api.Resource.User.UserDetailJM where

import Data.Aeson

import Shared.Api.Resource.User.RoleSimpleJM ()
import Shared.Api.Resource.User.UserDetailDTO
import Shared.Api.Resource.User.UserOpenIdIdentityJM ()
import Shared.Util.Aeson

instance FromJSON UserDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserDetailDTO where
  toJSON = genericToJSON jsonOptions
