module Shared.Database.Mapping.DocumentTemplate.Locale.DocumentTemplateLocale where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocale

instance ToRow DocumentTemplateLocale where
  toRow DocumentTemplateLocale {..} =
    [ toField uuid
    , toField name
    , toField code
    , toField documentTemplateUuid
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow DocumentTemplateLocale where
  fromRow = do
    uuid <- field
    name <- field
    code <- field
    documentTemplateUuid <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ DocumentTemplateLocale {..}
