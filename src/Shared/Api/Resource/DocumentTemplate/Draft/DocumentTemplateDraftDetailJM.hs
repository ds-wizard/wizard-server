module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Shared.Api.Resource.Project.ProjectSuggestionJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDraftDetail where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDetail where
  toJSON = genericToJSON jsonOptions
