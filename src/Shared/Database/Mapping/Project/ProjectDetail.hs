module Shared.Database.Mapping.Project.ProjectDetail where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types

import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Database.Mapping.Project.ProjectAcl
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectState ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.Project.Detail.ProjectDetail

instance FromRow ProjectDetail where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackage <- fieldKnowledgeModelPackageSuggestion
    selectedQuestionTagUuids <- fromPGArray <$> field
    isTemplate <- field
    permissions <- loadPermissions uuid
    fileCount <- field
    return $ ProjectDetail {..}
