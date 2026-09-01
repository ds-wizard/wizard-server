module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Api.Resource.Registry.RegistryOrganizationSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Util.Swagger

instance ToSchema KnowledgeModelPackageSimpleDTO where
  declareNamedSchema = toSwagger (toSimpleDTO globalKmPackage)

instance ToSchema KnowledgeModelPackageSimple where
  declareNamedSchema = toSwagger (toSimple globalKmPackage)
