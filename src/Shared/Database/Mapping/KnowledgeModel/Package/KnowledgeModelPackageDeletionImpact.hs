module Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM ()
import Shared.Api.Resource.Project.ProjectSimpleJM ()
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact

instance FromRow KnowledgeModelPackageDeletionImpact where
  fromRow = do
    uuid <- field
    name <- field
    version <- field
    packages <- fieldWith fromJSONField
    editors <- fieldWith fromJSONField
    projects <- fieldWith fromJSONField
    return $ KnowledgeModelPackageDeletionImpact {..}

instance FromRow KnowledgeModelPackageReference
