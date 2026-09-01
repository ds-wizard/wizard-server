module Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageSM where

import Data.Swagger

import Shared.Api.Resource.Coordinate.CoordinateSM ()
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Util.Swagger

instance ToSchema KnowledgeModelBundlePackage where
  declareNamedSchema = toSwagger globalKmBundlePackage
