module Shared.Api.Resource.User.RoleSimpleJM where

import Data.Aeson

import Shared.Model.User.RoleSimple

instance ToJSON RoleSimple

instance FromJSON RoleSimple
