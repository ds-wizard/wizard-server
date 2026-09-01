module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDraftCreateDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftCreateDTO
