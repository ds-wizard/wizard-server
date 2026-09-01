module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePatternJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Shared.Util.Swagger

instance ToSchema KnowledgeModelPackagePattern where
  declareNamedSchema = toSwagger kmPackagePatternAll
