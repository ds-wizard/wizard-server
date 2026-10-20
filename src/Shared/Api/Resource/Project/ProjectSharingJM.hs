module Shared.Api.Resource.Project.ProjectSharingJM where

import Data.Aeson

import Shared.Model.Project.Project
import Shared.Util.JsonSchema

instance FromJSON ProjectSharing

instance ToJSON ProjectSharing

instance JsonSchema ProjectSharing where
  jsonSchema = genericJsonSchema defaultOptions
