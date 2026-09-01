module Shared.Database.Mapping.DocumentTemplate.DocumentTemplateList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Database.Mapping.Common.SemVer2Tuple ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateState ()
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Model.DocumentTemplate.DocumentTemplateList

instance FromRow DocumentTemplateList where
  fromRow = do
    uuid <- field
    name <- field
    organizationId <- field
    templateId <- field
    version <- field
    phase <- field
    metamodelVersion <- field
    description <- field
    allowedPackages <- fieldWith fromJSONField
    nonEditable <- field
    language <- field
    potFileReady <- field
    remoteVersion <- field
    remoteOrganizationName <- field
    remoteOrganizationLogo <- field
    createdAt <- field
    return $ DocumentTemplateList {..}
