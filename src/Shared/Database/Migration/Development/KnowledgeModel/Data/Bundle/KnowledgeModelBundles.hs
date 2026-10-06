module Shared.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles where

import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

netherlandsV2KmBundle :: KnowledgeModelBundle
netherlandsV2KmBundle =
  KnowledgeModelBundle
    { name = netherlandsKmPackageV2.name
    , id = netherlandsKmPackageV2.id
    , version = netherlandsKmPackageV2.version
    , metamodelVersion = netherlandsKmPackageV2.metamodelVersion
    , packages = [globalKmBundlePackage, netherlandsKmBundlePackage, netherlandsV2KmBundlePackage]
    }

globalKmBundlePackage :: KnowledgeModelBundlePackage
globalKmBundlePackage = toKnowledgeModelBundlePackage globalKmPackage globalKmPackageEvents Nothing

netherlandsKmBundlePackage :: KnowledgeModelBundlePackage
netherlandsKmBundlePackage = toKnowledgeModelBundlePackage netherlandsKmPackage netherlandsKmPackageEvents (Just . createCoordinate $ globalKmPackage)

netherlandsV2KmBundlePackage :: KnowledgeModelBundlePackage
netherlandsV2KmBundlePackage = toKnowledgeModelBundlePackage netherlandsKmPackageV2 netherlandsKmPackageV2Events (Just . createCoordinate $ netherlandsKmPackage)
