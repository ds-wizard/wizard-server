module Shared.Database.Mapping.Project.Project where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.Project.Project

instance ToRow Project where
  toRow Project {..} =
    [ toField uuid
    , toField name
    , toField visibility
    , toField sharing
    , toField knowledgeModelPackageUuid
    , toField . PGArray $ selectedQuestionTagUuids
    , toField documentTemplateUuid
    , toField formatUuid
    , toField creatorUuid
    , toField createdAt
    , toField updatedAt
    , toField description
    , toField isTemplate
    , toField squashed
    , toField tenantUuid
    , toField . PGArray $ projectTags
    , toField language
    , toField documentTemplateLanguage
    ]

instance FromRow Project where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackageUuid <- field
    selectedQuestionTagUuids <- fromPGArray <$> field
    documentTemplateUuid <- field
    formatUuid <- field
    creatorUuid <- field
    let permissions = []
    createdAt <- field
    updatedAt <- field
    description <- field
    isTemplate <- field
    squashed <- field
    tenantUuid <- field
    projectTags <- fromPGArray <$> field
    language <- field
    documentTemplateLanguage <- field
    return $ Project {..}
