module Shared.Api.Resource.User.RoleSimpleJM where

import Data.Aeson

import Shared.Model.User.RoleSimple
import Shared.Util.JsonSchema

instance ToJSON RoleSimple

instance FromJSON RoleSimple

instance JsonSchema RoleSimple where
  jsonSchema = genericJsonSchema defaultOptions
