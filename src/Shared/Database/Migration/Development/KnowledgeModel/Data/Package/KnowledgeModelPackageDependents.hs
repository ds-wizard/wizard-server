module Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents where

import Shared.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Library.LibraryDependents
import Shared.Model.Project.Project

netherlandsKmPackageDependents :: LibraryDependents
netherlandsKmPackageDependents =
  LibraryDependents
    { knowledgeModelPackages = [netherlandsKmPackageV2Dependent]
    , editors = [amsterdamKnowledgeModelEditorDependent]
    , projects = [project4Dependent]
    , documents = []
    , hidden = noHiddenDependents
    , deleteAllowed = True
    }

netherlandsKmPackageHiddenDependents :: LibraryHiddenDependents
netherlandsKmPackageHiddenDependents = noHiddenDependents {projects = 1, workspaces = 1}

noHiddenDependents :: LibraryHiddenDependents
noHiddenDependents =
  LibraryHiddenDependents
    { knowledgeModelPackages = 0
    , editors = 0
    , projects = 0
    , documents = 0
    , workspaces = 0
    }

netherlandsKmPackageV2Dependent :: LibraryDependentKnowledgeModelPackage
netherlandsKmPackageV2Dependent =
  LibraryDependentKnowledgeModelPackage
    { uuid = netherlandsKmPackageV2.uuid
    , pId = netherlandsKmPackageV2.id
    , name = netherlandsKmPackageV2.name
    , version = netherlandsKmPackageV2.version
    }

amsterdamKnowledgeModelEditorDependent :: LibraryDependentResource
amsterdamKnowledgeModelEditorDependent =
  LibraryDependentResource
    { uuid = amsterdamKnowledgeModelEditor.uuid
    , name = amsterdamKnowledgeModelEditor.name
    }

project4Dependent :: LibraryDependentResource
project4Dependent =
  LibraryDependentResource
    { uuid = project4.uuid
    , name = project4.name
    }
