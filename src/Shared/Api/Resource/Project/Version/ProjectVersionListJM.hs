module Shared.Api.Resource.Project.Version.ProjectVersionListJM where

import Data.Aeson

import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Util.Aeson

instance FromJSON ProjectVersion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersion where
  toJSON = genericToJSON jsonOptions

instance FromJSON ProjectVersionList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersionList where
  toJSON = genericToJSON jsonOptions
