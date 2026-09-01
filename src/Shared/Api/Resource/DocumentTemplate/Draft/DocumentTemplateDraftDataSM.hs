module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Shared.Api.Resource.Project.ProjectSuggestionSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDraftDataDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftDataDTO
