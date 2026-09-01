module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDraftList where
  declareNamedSchema = toSwagger (toDraftList wizardDocumentTemplate)
