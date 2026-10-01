module Shared.Api.Resource.Library.LibraryDependentsSM where

import Data.Swagger

import Shared.Api.Resource.Library.LibraryDependentsJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents
import Shared.Model.Library.LibraryDependents
import Shared.Util.Swagger

instance ToSchema LibraryDependents where
  declareNamedSchema = toSwagger netherlandsKmPackageDependents

instance ToSchema LibraryDependentKnowledgeModelPackage where
  declareNamedSchema = toSwagger netherlandsKmPackageV2Dependent

instance ToSchema LibraryDependentResource where
  declareNamedSchema = toSwagger amsterdamKnowledgeModelEditorDependent

instance ToSchema LibraryHiddenDependents where
  declareNamedSchema = toSwagger netherlandsKmPackageHiddenDependents
