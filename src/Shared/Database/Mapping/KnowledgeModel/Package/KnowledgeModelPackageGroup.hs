module Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageGroup where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageGroup

instance FromRow KnowledgeModelPackageGroup where
  fromRow = do
    organizationId <- field
    kmId <- field
    versions <- field
    return $ KnowledgeModelPackageGroup {..}
