module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternSM ()
import Shared.Api.Resource.Project.ProjectSuggestionSM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplateDraftDetail
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDraftDetail where
  declareNamedSchema = toSwagger (toDraftDetail wizardDocumentTemplateDraft wizardDocumentTemplateFormats wizardDocumentTemplateDraftData Nothing Nothing)
