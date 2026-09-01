module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailSM where

import Data.Swagger

import Shared.Api.Resource.Coordinate.CoordinateSM ()
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.Api.Resource.Registry.RegistryOrganizationSM ()
import Shared.Api.Resource.Version.VersionSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Shared.Database.Migration.Development.Registry.Data.RegistryPackages
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Util.Swagger
import Shared.Util.Uuid

instance ToSchema KnowledgeModelPackageDetailDTO where
  declareNamedSchema =
    toSwagger
      ( toDetailDTO
          globalKmPackage
          False
          [globalRegistryPackage]
          [globalRegistryOrganization]
          [(u' "ac3a6934-2069-4792-943c-e1170edee8c2", "1.0.0")]
          (Just "https://registry.example.org")
          [czechGlobalKmLocaleList]
      )
