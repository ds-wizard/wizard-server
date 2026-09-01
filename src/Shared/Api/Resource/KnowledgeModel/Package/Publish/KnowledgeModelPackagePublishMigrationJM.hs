module Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Shared.Util.Aeson

instance FromJSON PackagePublishMigrationDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePublishMigrationDTO where
  toJSON = genericToJSON jsonOptions
