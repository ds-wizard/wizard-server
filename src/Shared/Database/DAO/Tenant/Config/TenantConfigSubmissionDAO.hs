module Shared.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Tenant.Config.TenantConfigSubmission ()
import Shared.Database.Mapping.Tenant.Config.TenantConfigSubmissionServiceSimple ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.TenantConfigSubmissionServiceSimple
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Util.String

-- --------------------------------------------------------
-- SUBMISSION
-- --------------------------------------------------------
findTenantConfigSubmission :: WizardRequestContextC s m => m TenantConfigSubmission
findTenantConfigSubmission = do
  tenantUuid <- asks (.tenantUuid')
  findTenantConfigSubmissionByUuid tenantUuid

findTenantConfigSubmissionByUuid :: WizardRequestContextC s m => U.UUID -> m TenantConfigSubmission
findTenantConfigSubmissionByUuid tenantUuid = do
  tcSubmission <- createFindEntityByFn "config_submission" [("tenant_uuid", U.toString tenantUuid)]
  tcSubmissionServices <- findTenantConfigSubmissionServices
  return $ tcSubmission {services = tcSubmissionServices}

insertTenantConfigSubmission :: WizardRequestContextC s m => TenantConfigSubmission -> m ()
insertTenantConfigSubmission submission = do
  createInsertFn "config_submission" submission
  traverse_ insertOrUpdateConfigSubmissionService submission.services

updateTenantConfigSubmission :: WizardRequestContextC s m => TenantConfigSubmission -> m Int64
updateTenantConfigSubmission submission = do
  let sql = "UPDATE config_submission SET tenant_uuid = ?, enabled = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ?;"
  let params = toRow submission ++ [toField submission.tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  -- Update services
  traverse_ insertOrUpdateConfigSubmissionService submission.services
  deleteTenantConfigSubmissionsExcept (fmap (.sId) submission.services)

deleteTenantConfigSubmissions :: WizardRequestContextC s m => m Int64
deleteTenantConfigSubmissions = createDeleteEntitiesFn "config_submission"

-- --------------------------------------------------------
-- SUBMISSION SERVICE
-- --------------------------------------------------------
findTenantConfigSubmissionServices :: WizardRequestContextC s m => m [TenantConfigSubmissionService]
findTenantConfigSubmissionServices = do
  tenantUuid <- asks (.tenantUuid')
  tcSubmissionServices <- createFindEntitiesByFn "config_submission_service" [("tenant_uuid", U.toString tenantUuid)]
  traverse
    ( \service -> do
        supportedFormats <- findTenantConfigSubmissionServiceSupportedFormats service.sId
        requestHeaders <- findTenantConfigSubmissionServiceRequestHeaders service.sId
        return $
          service
            { supportedFormats = supportedFormats
            , request = service.request {headers = requestHeaders}
            }
    )
    tcSubmissionServices

findTenantConfigSubmissionServicesByDocumentTemplateUuidAndFormatUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m [TenantConfigSubmissionServiceSimple]
findTenantConfigSubmissionServicesByDocumentTemplateUuidAndFormatUuid documentTemplateUuid formatUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT DISTINCT service.id, \
          \                service.name, \
          \                service.description \
          \FROM config_submission_service service \
          \     JOIN config_submission_service_supported_format supported_format \
          \          ON service.tenant_uuid = supported_format.tenant_uuid AND \
          \             service.id = supported_format.service_id \
          \WHERE service.tenant_uuid = ? \
          \  AND supported_format.document_template_uuid = ? \
          \  AND supported_format.format_uuid = ? \
          \ORDER BY service.id;"
  let params = [toField tenantUuid, toField documentTemplateUuid, toField formatUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findTenantConfigSubmissionServiceByServiceId :: WizardRequestContextC s m => String -> m TenantConfigSubmissionService
findTenantConfigSubmissionServiceByServiceId serviceId = do
  tenantUuid <- asks (.tenantUuid')
  service <- createFindEntityByFn "config_submission_service" [("tenant_uuid", U.toString tenantUuid), ("id", serviceId)]
  supportedFormats <- findTenantConfigSubmissionServiceSupportedFormats service.sId
  requestHeaders <- findTenantConfigSubmissionServiceRequestHeaders service.sId
  return $
    service
      { supportedFormats = supportedFormats
      , request = service.request {headers = requestHeaders}
      }

insertOrUpdateConfigSubmissionService :: WizardRequestContextC s m => TenantConfigSubmissionService -> m Int64
insertOrUpdateConfigSubmissionService service = do
  let sql =
        fromString
          "INSERT INTO config_submission_service \
          \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
          \ON CONFLICT (tenant_uuid, id) DO UPDATE SET tenant_uuid                 = ?, \
          \                                            id                          = ?, \
          \                                            name                        = ?, \
          \                                            description                 = ?, \
          \                                            props                       = ?, \
          \                                            request_method              = ?, \
          \                                            request_url                 = ?, \
          \                                            request_multipart_enabled   = ?, \
          \                                            request_multipart_file_name = ?, \
          \                                            created_at                  = ?, \
          \                                            updated_at                  = ?;"
  let params = toRow service ++ toRow service
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  -- Update request headers
  traverse_ (insertOrUpdateConfigSubmissionServiceRequestHeader service.tenantUuid service.sId) (M.toList service.request.headers)
  deleteTenantConfigSubmissionsRequestHeaderExcept service.sId (M.keys service.request.headers)
  -- Update supported formats
  traverse_ insertOrUpdateConfigSubmissionServiceSupportedFormat service.supportedFormats
  deleteTenantConfigSubmissionsSupportedFormatExcept service.sId service.supportedFormats

deleteTenantConfigSubmissionsExcept :: WizardRequestContextC s m => [String] -> m Int64
deleteTenantConfigSubmissionsExcept serviceIds = do
  let serviceIdCondition =
        case serviceIds of
          [] -> ""
          _ -> f' "AND id NOT IN (%s)" [generateQuestionMarks serviceIds]
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "DELETE FROM config_submission_service \
            \WHERE tenant_uuid = ? %s"
            [serviceIdCondition]
  let params = U.toString tenantUuid : serviceIds
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

-- --------------------------------------------------------
-- SUBMISSION SERVICE SUPPORTED FORMAT
-- --------------------------------------------------------
findTenantConfigSubmissionServiceSupportedFormats :: WizardRequestContextC s m => String -> m [TenantConfigSubmissionServiceSupportedFormat]
findTenantConfigSubmissionServiceSupportedFormats serviceId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn "config_submission_service_supported_format" [("tenant_uuid", U.toString tenantUuid), ("service_id", serviceId)]

insertOrUpdateConfigSubmissionServiceSupportedFormat :: WizardRequestContextC s m => TenantConfigSubmissionServiceSupportedFormat -> m Int64
insertOrUpdateConfigSubmissionServiceSupportedFormat supportedFormat = do
  let sql =
        fromString
          "INSERT INTO config_submission_service_supported_format \
          \VALUES (?, ?, ?, ?) \
          \ON CONFLICT (tenant_uuid, service_id, document_template_uuid, format_uuid) DO NOTHING"
  let params = toRow supportedFormat
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigSubmissionsSupportedFormatExcept :: WizardRequestContextC s m => String -> [TenantConfigSubmissionServiceSupportedFormat] -> m Int64
deleteTenantConfigSubmissionsSupportedFormatExcept serviceId supportedFormats = do
  tenantUuid <- asks (.tenantUuid')
  let condition =
        case supportedFormats of
          [] -> ""
          _ ->
            f'
              "AND NOT (%s)"
              [L.intercalate " OR " (replicate (length supportedFormats) "(document_template_uuid = ? AND format_uuid = ?)")]
  let sql = fromString $ f' "DELETE FROM config_submission_service_supported_format WHERE tenant_uuid = ? AND service_id = ? %s" [condition]
  let params = [toField tenantUuid, toField serviceId] ++ concatMap (\supportedFormat -> [toField supportedFormat.templateUuid, toField supportedFormat.formatUuid]) supportedFormats
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

-- --------------------------------------------------------
-- SUBMISSION SERVICE REQUEST HEADER
-- --------------------------------------------------------
findTenantConfigSubmissionServiceRequestHeaders :: WizardRequestContextC s m => String -> m (M.Map String String)
findTenantConfigSubmissionServiceRequestHeaders serviceId = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT name, value \
          \FROM config_submission_service_request_header \
          \WHERE tenant_uuid = ? AND service_id = ? \
          \GROUP BY name, value"
  let params = [toField tenantUuid, toField serviceId]
  logQuery sql params
  let action conn = query conn sql params
  results <- runDB action
  return . M.fromList $ results

insertOrUpdateConfigSubmissionServiceRequestHeader :: WizardRequestContextC s m => U.UUID -> String -> (String, String) -> m Int64
insertOrUpdateConfigSubmissionServiceRequestHeader tenantUuid serviceId (name, value) = do
  let sql =
        fromString
          "INSERT INTO config_submission_service_request_header \
          \VALUES (?, ?, ?, ?) \
          \ON CONFLICT (tenant_uuid, service_id, name) DO UPDATE SET value = ?"
  let params = [U.toString tenantUuid, serviceId, name, value, value]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTenantConfigSubmissionsRequestHeaderExcept :: WizardRequestContextC s m => String -> [String] -> m Int64
deleteTenantConfigSubmissionsRequestHeaderExcept serviceId names = do
  tenantUuid <- asks (.tenantUuid')
  let condition =
        case names of
          [] -> ""
          _ ->
            f'
              "AND NOT (%s)"
              [L.intercalate " OR " (replicate (length names) "name = ?")]
  let sql = fromString $ f' "DELETE FROM config_submission_service_request_header WHERE tenant_uuid = ? AND service_id = ? %s" [condition]
  let params = [toField tenantUuid, toField serviceId] ++ fmap toField names
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
