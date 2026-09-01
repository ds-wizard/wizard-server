module Shared.Database.Mapping.DocumentTemplate.DocumentTemplate where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Database.Mapping.Common.SemVer2Tuple ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()

instance ToRow DocumentTemplate where
  toRow DocumentTemplate {..} =
    [ toField uuid
    , toField name
    , toField organizationId
    , toField templateId
    , toField version
    , toField metamodelVersion
    , toField description
    , toField readme
    , toField license
    , toJSONField allowedPackages
    , toField createdAt
    , toField tenantUuid
    , toField updatedAt
    , toField phase
    , toField nonEditable
    , toField language
    , toField potFileReady
    ]

instance FromRow DocumentTemplate where
  fromRow = do
    uuid <- field
    name <- field
    organizationId <- field
    templateId <- field
    version <- field
    metamodelVersion <- field
    description <- field
    readme <- field
    license <- field
    allowedPackages <- fieldWith fromJSONField
    createdAt <- field
    tenantUuid <- field
    updatedAt <- field
    phase <- field
    nonEditable <- field
    language <- field
    potFileReady <- field
    return $ DocumentTemplate {..}
