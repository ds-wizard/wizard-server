module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionSM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM ()
import Shared.Api.Resource.Project.ProjectSimpleSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Shared.Util.Swagger

instance ToSchema KnowledgeModelPackageDeletionImpact where
  declareNamedSchema = toSwagger netherlandsKmPackageDeletionImpact

instance ToSchema KnowledgeModelPackageReference where
  declareNamedSchema = toSwagger netherlandsKmPackageReference
