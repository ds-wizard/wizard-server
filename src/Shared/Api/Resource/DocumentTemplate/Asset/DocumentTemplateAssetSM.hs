module Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetSM where

import Data.Swagger

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetDTO
import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetJM ()
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.Asset.DocumentTemplateAssetMapper
import Shared.Util.Swagger

instance ToSchema DocumentTemplateAssetDTO where
  declareNamedSchema = toSwagger (toDTO assetLogo "https://s3.com/asset-logo.png" assetLogo.createdAt)
