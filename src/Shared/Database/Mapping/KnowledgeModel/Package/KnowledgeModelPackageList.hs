module Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageList where

import Database.PostgreSQL.Simple

import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackagePhase ()
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageList

instance FromRow KnowledgeModelPackageList
