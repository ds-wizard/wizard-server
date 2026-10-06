module Shared.Database.DAO.Settings.SettingsSubmissionDAO where

import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.ToField

import Shared.Database.DAO.Settings.SettingsDAO
import Shared.Database.Mapping.Settings.SettingsSubmission ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

table :: String
table = "settings_submission"

serviceTable :: String
serviceTable = "settings_submission_service"

headerTable :: String
headerTable = "settings_submission_service_request_header"

supportedFormatTable :: String
supportedFormatTable = "settings_submission_service_supported_format"

childTables :: [String]
childTables = [headerTable, supportedFormatTable, serviceTable]

serviceColumns :: [String]
serviceColumns = ["id", "name", "description", "props", "request_method", "request_url", "request_multipart_enabled", "request_multipart_file_name"]

findSettingsSubmission :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> m (Maybe SettingsSubmission)
findSettingsSubmission tenantUuid mWorkspaceUuid = do
  let key = scopedKey table tenantUuid mWorkspaceUuid
  mSettings <- findSettingsRow ["enabled"] key
  case mSettings of
    Nothing -> return Nothing
    Just settings -> do
      services <- findSettingsRows serviceColumns "ORDER BY id" (childKey serviceTable key)
      supportedFormats <- groupByServiceId <$> findSettingsRows ["service_id", "id", "version", "format_name"] "ORDER BY id, version, format_name" (childKey supportedFormatTable key)
      headers <- groupByServiceId <$> findSettingsRows ["service_id", "name", "value"] "ORDER BY name" (childKey headerTable key)
      return . Just $ settings {services = fmap (withServiceChildren supportedFormats headers) services}

withServiceChildren :: M.Map String [SettingsSubmissionServiceSupportedFormat] -> M.Map String [(String, String)] -> SettingsSubmissionService -> SettingsSubmissionService
withServiceChildren supportedFormats headers service =
  service
    { supportedFormats = M.findWithDefault [] service.sId supportedFormats
    , request = service.request {headers = M.fromList (M.findWithDefault [] service.sId headers)}
    }

groupByServiceId :: [Only String :. a] -> M.Map String [a]
groupByServiceId rows = M.fromListWith (flip (++)) [(serviceId, [row]) | Only serviceId :. row <- rows]

saveSettingsSubmission :: WizardRequestContextC s m => U.UUID -> Maybe U.UUID -> SettingsSubmission -> m ()
saveSettingsSubmission tenantUuid mWorkspaceUuid settings = do
  let key = scopedKey table tenantUuid mWorkspaceUuid
  saveSettingsRow ["enabled"] key settings
  forM_ childTables (deleteSettingsRowsByKey . (`childKey` key))
  forM_ settings.services $ \service -> do
    insertSettingsRow serviceColumns (childKey serviceTable key) service
    let serviceKey = withServiceId service.sId key
    forM_ service.supportedFormats (insertSettingsRow ["id", "version", "format_name"] (childKey supportedFormatTable serviceKey))
    forM_ (M.toList service.request.headers) (insertSettingsRow ["name", "value"] (childKey headerTable serviceKey))

withServiceId :: String -> SettingsKey -> SettingsKey
withServiceId serviceId key = key {columns = key.columns ++ [("service_id", toField serviceId)]}
