module Shared.Api.Resource.Project.ProjectStateJM where

import Data.Aeson

import Shared.Model.Project.ProjectState

instance FromJSON KnowledgeModelProjectState

instance ToJSON KnowledgeModelProjectState

instance FromJSON DocumentTemplateProjectState

instance ToJSON DocumentTemplateProjectState
