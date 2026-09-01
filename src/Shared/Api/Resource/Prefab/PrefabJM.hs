module Shared.Api.Resource.Prefab.PrefabJM where

import Data.Aeson

import Shared.Model.Prefab.Prefab
import Shared.Util.Aeson

instance FromJSON Prefab where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Prefab where
  toJSON = genericToJSON jsonOptions
