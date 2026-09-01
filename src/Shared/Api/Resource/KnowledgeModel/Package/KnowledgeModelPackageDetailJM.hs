module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM where

import Data.Aeson

import Shared.Api.Resource.Coordinate.CoordinateJM ()
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseJM ()
import Shared.Api.Resource.Registry.RegistryOrganizationJM ()
import Shared.Api.Resource.Version.VersionJM ()
import Shared.Util.Aeson

instance FromJSON KnowledgeModelPackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageDetailDTO where
  toJSON = genericToJSON jsonOptions
