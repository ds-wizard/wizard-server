module Shared.Database.DAO.Submission.SubmissionDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Submission.Submission ()
import Shared.Database.Mapping.Submission.SubmissionList ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionList

entityName = "submission"

pageLabel = "submissions"

findSubmissions :: WizardRequestContextC s m => m [Submission]
findSubmissions = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findSubmissionsFiltered :: WizardRequestContextC s m => [(String, String)] -> m [Submission]
findSubmissionsFiltered params = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

findSubmissionsByDocumentUuid :: WizardRequestContextC s m => U.UUID -> m [SubmissionList]
findSubmissionsByDocumentUuid documentUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT submission.uuid, \
          \       submission.state, \
          \       submission.location, \
          \       submission.returned_data, \
          \       submission.document_uuid, \
          \       submission.created_at, \
          \       submission.updated_at, \
          \       service.id, \
          \       service.name, \
          \       user_entity.uuid, \
          \       user_entity.first_name, \
          \       user_entity.last_name, \
          \       gravatar_hash(user_entity.email), \
          \       user_entity.image_url, \
          \       user_entity.affiliation \
          \FROM submission \
          \LEFT JOIN config_submission_service service ON service.tenant_uuid = submission.tenant_uuid AND service.id = submission.service_id \
          \LEFT JOIN user_entity ON user_entity.tenant_uuid = submission.tenant_uuid AND user_entity.uuid = submission.created_by \
          \WHERE submission.tenant_uuid = ? AND document_uuid = ? \
          \ORDER BY submission.created_at DESC"
  let params = [toField tenantUuid, toField documentUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertSubmission :: WizardRequestContextC s m => Submission -> m Int64
insertSubmission = createInsertFn entityName

updateSubmissionByUuid :: WizardRequestContextC s m => Submission -> m Submission
updateSubmissionByUuid sub = do
  now <- liftIO getCurrentTime
  tenantUuid <- asks (.tenantUuid')
  let updatedSub = sub {updatedAt = now} :: Submission
  let sql =
        fromString
          "UPDATE submission SET uuid = ?, state = ?, location = ?, returned_data = ?, service_id = ?, document_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow sub ++ [toField tenantUuid, toField updatedSub.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedSub

deleteSubmissions :: WizardRequestContextC s m => m Int64
deleteSubmissions = createDeleteEntitiesFn entityName
