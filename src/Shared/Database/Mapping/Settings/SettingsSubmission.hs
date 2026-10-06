module Shared.Database.Mapping.Settings.SettingsSubmission where

import qualified Data.Map.Strict as M
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Shared.Model.Settings.Settings

instance ToRow SettingsSubmission where
  toRow SettingsSubmission {..} = [toField enabled]

instance FromRow SettingsSubmission where
  fromRow = do
    enabled <- field
    let services = []
    return SettingsSubmission {..}

instance ToRow SettingsSubmissionService where
  toRow SettingsSubmissionService {..} =
    [ toField sId
    , toField name
    , toField description
    , toField . PGArray $ props
    , toField request.method
    , toField request.url
    , toField request.multipart.enabled
    , toField request.multipart.fileName
    ]

instance FromRow SettingsSubmissionService where
  fromRow = do
    sId <- field
    name <- field
    description <- field
    props <- fromPGArray <$> field
    let supportedFormats = []
    method <- field
    url <- field
    multipart <- SettingsSubmissionServiceRequestMultipart <$> field <*> field
    let request = SettingsSubmissionServiceRequest {headers = M.empty, ..}
    return SettingsSubmissionService {..}

instance ToRow SettingsSubmissionServiceSupportedFormat

instance FromRow SettingsSubmissionServiceSupportedFormat
