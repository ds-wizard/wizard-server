module Wizard.Database.DAO.Submission.SubmissionDAO where

import Control.Monad.Reader (asks, liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Submission.Submission ()
import Wizard.Database.Mapping.Submission.SubmissionList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Submission.Submission
import Wizard.Model.Submission.SubmissionList

entityName = "w_submission"

pageLabel = "submissions"

findSubmissions :: AppContextM [Submission]
findSubmissions = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findSubmissionsFiltered :: [(String, String)] -> AppContextM [Submission]
findSubmissionsFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

findSubmissionsByDocumentUuid :: U.UUID -> AppContextM [SubmissionList]
findSubmissionsByDocumentUuid documentUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "SELECT w_submission.uuid, \
          \       w_submission.state, \
          \       w_submission.location, \
          \       w_submission.returned_data, \
          \       w_submission.document_uuid, \
          \       w_submission.created_at, \
          \       w_submission.updated_at, \
          \       service.id, \
          \       service.name, \
          \       w_user_entity.uuid, \
          \       w_user_entity.first_name, \
          \       w_user_entity.last_name, \
          \       w_gravatar_hash(w_user_entity.email), \
          \       w_user_entity.image_url, \
          \       w_user_entity.affiliation \
          \FROM w_submission \
          \LEFT JOIN w_config_submission_service service ON service.tenant_uuid = w_submission.tenant_uuid AND service.id = w_submission.service_id \
          \LEFT JOIN w_user_entity ON w_user_entity.tenant_uuid = w_submission.tenant_uuid AND w_user_entity.uuid = w_submission.created_by \
          \WHERE w_submission.tenant_uuid = ? AND document_uuid = ? \
          \ORDER BY w_submission.created_at DESC"
  let params = [toField tenantUuid, toField documentUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertSubmission :: Submission -> AppContextM Int64
insertSubmission = createInsertFn entityName

updateSubmissionByUuid :: Submission -> AppContextM Submission
updateSubmissionByUuid sub = do
  now <- liftIO getCurrentTime
  tenantUuid <- asks currentTenantUuid
  let updatedSub = sub {updatedAt = now} :: Submission
  let sql =
        fromString
          "UPDATE w_submission SET uuid = ?, state = ?, location = ?, returned_data = ?, service_id = ?, document_uuid = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow sub ++ [toField tenantUuid, toField updatedSub.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedSub

deleteSubmissions :: AppContextM Int64
deleteSubmissions = createDeleteEntitiesFn entityName
