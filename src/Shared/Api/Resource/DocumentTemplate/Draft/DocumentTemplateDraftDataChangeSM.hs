module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import Shared.Util.Swagger

instance ToSchema DocumentTemplateDraftDataChangeDTO where
  declareNamedSchema = toSwagger wizardDocumentTemplateDraftDataChangeDTO
