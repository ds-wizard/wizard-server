module Shared.Database.Mapping.DocumentTemplate.DocumentTemplateSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Database.Mapping.Common.SemVer2Tuple ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateState ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateSuggestion

instance FromRow DocumentTemplateSuggestion where
  fromRow = do
    uuid <- field
    name <- field
    organizationId <- field
    templateId <- field
    version <- field
    phase <- field
    metamodelVersion <- field
    description <- field
    language <- field
    allowedPackages <- fieldWith fromJSONField
    formats <- fieldWith fromJSONField
    locales <- fieldWith fromJSONField
    return $ DocumentTemplateSuggestion {..}
