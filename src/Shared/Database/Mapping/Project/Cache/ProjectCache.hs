module Shared.Database.Mapping.Project.Cache.ProjectCache where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Shared.Api.Resource.Project.Detail.ProjectDetailReportJM ()
import Shared.Api.Resource.Project.Version.ProjectVersionListJM ()
import Shared.Model.Project.Cache.ProjectCache

instance ToRow ProjectCache where
  toRow ProjectCache {..} =
    [ toField projectUuid
    , toJSONField questionnaire
    , toJSONField report
    , toJSONField versions
    , toField questionnaireSourceUpdatedAt
    , toField versionsSourceUpdatedAt
    , toField tenantUuid
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow ProjectCache where
  fromRow = do
    projectUuid <- field
    questionnaire <- fieldWith fromJSONField
    report <- fieldWith fromJSONField
    versions <- fieldWith fromJSONField
    questionnaireSourceUpdatedAt <- field
    versionsSourceUpdatedAt <- field
    tenantUuid <- field
    createdAt <- field
    updatedAt <- field
    return $ ProjectCache {..}
