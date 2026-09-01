module Shared.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplates
import Shared.Model.DocumentTemplate.DocumentTemplateWithCoordinate
import Shared.Util.Swagger

instance ToSchema DocumentTemplateWithCoordinate where
  declareNamedSchema = toSwagger wizardDocumentTemplateWithCoordinate
