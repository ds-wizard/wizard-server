module Shared.Api.Resource.Project.ProjectStateSM where

import Data.Swagger

import Shared.Model.Project.ProjectState

instance ToSchema KnowledgeModelProjectState

instance ToSchema DocumentTemplateProjectState
