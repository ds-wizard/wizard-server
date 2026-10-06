module Shared.Database.Migration.Development.Registry.Data.RegistryPackages where

import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Registry.RegistryPackage

globalRegistryPackage :: RegistryPackage
globalRegistryPackage =
  RegistryPackage
    { id = globalKmPackage.id
    , remoteVersion = globalKmPackage.version
    , createdAt = globalKmPackage.createdAt
    }

nlRegistryPackage :: RegistryPackage
nlRegistryPackage =
  RegistryPackage
    { id = netherlandsKmPackageV2.id
    , remoteVersion = netherlandsKmPackageV2.version
    , createdAt = netherlandsKmPackageV2.createdAt
    }
