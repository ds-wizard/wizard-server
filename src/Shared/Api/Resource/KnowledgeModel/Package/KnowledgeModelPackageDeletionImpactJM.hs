module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Shared.Api.Resource.Project.ProjectSimpleJM ()
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Shared.Util.Aeson

instance FromJSON KnowledgeModelPackageDeletionImpact where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageDeletionImpact where
  toJSON = genericToJSON jsonOptions

instance FromJSON KnowledgeModelPackageReference where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelPackageReference where
  toJSON = genericToJSON jsonOptions
