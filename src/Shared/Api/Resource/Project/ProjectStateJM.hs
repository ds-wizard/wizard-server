module Shared.Api.Resource.Project.ProjectStateJM where

import Data.Aeson

import Shared.Model.Project.ProjectState
import Shared.Util.JsonSchema

instance FromJSON KnowledgeModelProjectState

instance ToJSON KnowledgeModelProjectState

instance FromJSON DocumentTemplateProjectState

instance ToJSON DocumentTemplateProjectState

instance JsonSchema KnowledgeModelProjectState where
  jsonSchema = genericJsonSchema defaultOptions

instance JsonSchema DocumentTemplateProjectState where
  jsonSchema = genericJsonSchema defaultOptions
