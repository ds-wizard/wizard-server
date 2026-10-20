module Shared.Api.Resource.Project.ProjectVisibilityJM where

import Data.Aeson

import Shared.Model.Project.Project
import Shared.Util.JsonSchema

instance FromJSON ProjectVisibility

instance ToJSON ProjectVisibility

instance JsonSchema ProjectVisibility where
  jsonSchema = genericJsonSchema defaultOptions
