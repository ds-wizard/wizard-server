module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Util.Swagger

instance ToSchema KnowledgeModelPackageChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO globalKmPackage)
