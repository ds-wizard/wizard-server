module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDraftChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftChangeDTO
