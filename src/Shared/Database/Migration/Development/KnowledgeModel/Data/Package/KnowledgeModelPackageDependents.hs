module Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents where

import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Shared.Service.Project.ProjectMapper

netherlandsKmPackageDeletionImpact :: KnowledgeModelPackageDeletionImpact
netherlandsKmPackageDeletionImpact =
  KnowledgeModelPackageDeletionImpact
    { uuid = netherlandsKmPackage.uuid
    , name = netherlandsKmPackage.name
    , version = netherlandsKmPackage.version
    , packages = [netherlandsKmPackageReference]
    , editors = [amsterdamKnowledgeModelEditorSuggestion]
    , projects = [toSimple project4]
    }

netherlandsKmPackageReference :: KnowledgeModelPackageReference
netherlandsKmPackageReference =
  KnowledgeModelPackageReference
    { uuid = netherlandsKmPackageV2.uuid
    , name = netherlandsKmPackageV2.name
    , version = netherlandsKmPackageV2.version
    }

netherlandsKmPackageV2DeletionImpact :: KnowledgeModelPackageDeletionImpact
netherlandsKmPackageV2DeletionImpact =
  KnowledgeModelPackageDeletionImpact
    { uuid = netherlandsKmPackageV2.uuid
    , name = netherlandsKmPackageV2.name
    , version = netherlandsKmPackageV2.version
    , packages = []
    , editors = []
    , projects = []
    }
