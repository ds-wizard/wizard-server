module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Shared.Api.Resource.Project.ProjectSuggestionJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDraftDataDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDataDTO where
  toJSON = genericToJSON jsonOptions
