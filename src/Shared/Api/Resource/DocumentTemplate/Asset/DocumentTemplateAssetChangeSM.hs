module Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplateAssets
import Shared.Util.Swagger

instance ToSchema DocumentTemplateAssetChangeDTO where
  declareNamedSchema = toSwagger assetLogoChangeDTO
