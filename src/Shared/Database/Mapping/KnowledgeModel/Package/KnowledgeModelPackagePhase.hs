module Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

instance ToField KnowledgeModelPackagePhase where
  toField = toFieldGenericEnum

instance FromField KnowledgeModelPackagePhase where
  fromField = fromFieldGenericEnum
