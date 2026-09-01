module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageMapper
import Shared.Util.Swagger

instance ToSchema KnowledgeModelPackageSuggestion where
  declareNamedSchema = toSwagger [toSuggestion globalKmPackage]
