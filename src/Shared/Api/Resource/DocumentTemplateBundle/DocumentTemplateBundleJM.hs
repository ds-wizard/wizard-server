module Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleJM where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL

import Shared.Api.Resource.Common.SemVer2TupleJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Api.Resource.Localization.LocaleRecordJM ()
import Shared.Constant.DocumentTemplate
import Shared.Localization.Messages.DocumentTemplate.Public
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance ToJSON DocumentTemplateBundleDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON DocumentTemplateBundleDTO where
  parseJSON (Object o) = do
    tId <- o .: "id"
    name <- o .: "name"
    organizationId <- o .: "organizationId"
    templateId <- o .: "templateId"
    version <- o .: "version"
    metamodelVersion <-
      (o .: "metamodelVersion" :: Parser SemVer2Tuple)
        <|> fail (BSL.unpack . encode $ _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION tId "<<unable-to-parse>>" (show documentTemplateMetamodelVersion))
    description <- o .: "description"
    readme <- o .: "readme"
    license <- o .: "license"
    allowedPackages <- o .: "allowedPackages"
    language <- o .:? "language" .!= "en"
    formats <- o .: "formats"
    files <- o .: "files"
    assets <- o .: "assets"
    createdAt <- o .: "createdAt"
    return DocumentTemplateBundleDTO {..}
  parseJSON _ = mzero
