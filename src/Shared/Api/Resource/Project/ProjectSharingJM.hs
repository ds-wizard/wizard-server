module Shared.Api.Resource.Project.ProjectSharingJM where

import Data.Aeson

import Shared.Model.Project.Project

instance FromJSON ProjectSharing

instance ToJSON ProjectSharing
