module Shared.Database.Mapping.Project.ProjectDetailPreview where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.Project.Acl.ProjectPermJM ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Database.Mapping.Project.ProjectAcl
import Shared.Database.Mapping.Project.ProjectSharing ()
import Shared.Database.Mapping.Project.ProjectVisibility ()
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Model.Project.Detail.ProjectDetailPreview

instance FromRow ProjectDetailPreview where
  fromRow = do
    uuid <- field
    name <- field
    visibility <- field
    sharing <- field
    knowledgeModelPackage <- fieldKnowledgeModelPackageSuggestion
    isTemplate <- field
    documentTemplateUuid <- field
    permissions <- loadPermissions uuid
    mFormatUuid <- field
    mFormatName <- field
    mFormatIcon <- field
    let format =
          case (mFormatUuid, mFormatName, mFormatIcon) of
            (Just uuid, Just name, Just icon) -> Just $ DocumentTemplateFormatSimple {uuid = uuid, name = name, icon = icon}
            _ -> Nothing
    fileCount <- field
    return $ ProjectDetailPreview {..}
