module Shared.Database.Migration.Development.DocumentTemplate.Data.WizardDocumentTemplateAssets where

import Shared.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeDTO
import Shared.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.Model.DocumentTemplate.DocumentTemplate

assetLogoChangeDTO :: DocumentTemplateAssetChangeDTO
assetLogoChangeDTO =
  DocumentTemplateAssetChangeDTO
    { fileName = assetLogoEdited.fileName
    }
