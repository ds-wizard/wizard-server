module Shared.Database.DAO.User.UserSubmissionPropDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.User.User ()
import Shared.Database.Mapping.User.UserSubmissionProp ()
import Shared.Database.Mapping.User.UserSubmissionPropList ()
import Shared.Database.Mapping.User.UserWithMembership ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.UserSubmissionProp
import Shared.Model.User.UserSubmissionPropList
import Shared.Util.String

entityName = "user_entity_submission_prop"

findUserSubmissionProps :: WizardRequestContextC s m => U.UUID -> m [UserSubmissionProp]
findUserSubmissionProps userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]

findUserSubmissionPropsList :: WizardRequestContextC s m => U.UUID -> m [UserSubmissionPropList]
findUserSubmissionPropsList userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT nested.id                                                          AS service_id, \
          \       nested.name                                                        AS service_name, \
          \       jsonb_object_agg(nested.key, coalesce((submission_prop.values ->> nested.key)::varchar, '')) AS props \
          \FROM (SELECT service.id, \
          \             service.name, \
          \             service.tenant_uuid, \
          \             unnest(service.props) AS key \
          \      FROM config_submission_service AS service \
          \      WHERE service.tenant_uuid = ?) AS nested \
          \LEFT JOIN user_entity_submission_prop AS submission_prop \
          \          ON submission_prop.service_id = nested.id AND \
          \             submission_prop.tenant_uuid = nested.tenant_uuid AND \
          \             submission_prop.user_uuid = ? \
          \GROUP BY nested.id, nested.name;"
  let params = [U.toString tenantUuid, U.toString userUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertOrUpdateUserSubmissionProp :: WizardRequestContextC s m => UserSubmissionProp -> m Int64
insertOrUpdateUserSubmissionProp submissionProps = do
  let sql =
        fromString
          "INSERT INTO user_entity_submission_prop \
          \VALUES (?, ?, ?, ?, ?, ?) \
          \ON CONFLICT (user_uuid, service_id) DO UPDATE SET user_uuid   = ?, \
          \                                                  service_id  = ?, \
          \                                                  values      = ?, \
          \                                                  tenant_uuid = ?, \
          \                                                  created_at  = ?, \
          \                                                  updated_at  = ?;"
  let params = toRow submissionProps ++ toRow submissionProps
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserSubmissionPropsExcept :: WizardRequestContextC s m => U.UUID -> [String] -> m Int64
deleteUserSubmissionPropsExcept userUuid serviceIds = do
  let serviceIdCondition =
        case serviceIds of
          [] -> ""
          _ -> f' "AND service_id NOT IN (%s)" [generateQuestionMarks serviceIds]
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "DELETE FROM user_entity_submission_prop \
            \WHERE tenant_uuid = ? AND user_uuid = ? %s"
            [serviceIdCondition]
  let params = U.toString tenantUuid : U.toString userUuid : serviceIds
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteUserSubmissionProps :: WizardRequestContextC s m => m Int64
deleteUserSubmissionProps = createDeleteEntitiesFn entityName

deleteUserByUserUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteUserByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]
