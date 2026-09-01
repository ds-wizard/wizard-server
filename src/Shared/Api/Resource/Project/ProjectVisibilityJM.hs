module Shared.Api.Resource.Project.ProjectVisibilityJM where

import Data.Aeson

import Shared.Model.Project.Project

instance FromJSON ProjectVisibility

instance ToJSON ProjectVisibility
