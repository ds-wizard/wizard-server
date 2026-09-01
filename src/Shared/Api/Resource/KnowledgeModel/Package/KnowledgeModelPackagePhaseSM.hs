module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackagePhaseSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToSchema KnowledgeModelPackagePhase

instance ToParamSchema KnowledgeModelPackagePhase
