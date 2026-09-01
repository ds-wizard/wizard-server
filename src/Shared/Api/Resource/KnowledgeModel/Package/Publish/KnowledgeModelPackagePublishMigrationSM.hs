module Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Util.Swagger

instance ToSchema PackagePublishMigrationDTO where
  declareNamedSchema = toSwagger packagePublishMigrationDTO
