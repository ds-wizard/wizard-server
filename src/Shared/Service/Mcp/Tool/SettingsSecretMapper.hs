module Shared.Service.Mcp.Tool.SettingsSecretMapper where

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Shared.Model.Error.Error
import Shared.Model.Settings.Settings
import Shared.Util.Secret

maskSubmission :: SettingsSubmission -> SettingsSubmission
maskSubmission settings = settings {services = fmap (\service -> withHeaders (maskSecrets service.request.headers) service) settings.services}

unmaskSubmission :: SettingsSubmission -> SettingsSubmission -> Either AppError SettingsSubmission
unmaskSubmission current settings = (\services -> settings {services = services}) <$> traverse (unmaskService current.services) settings.services

unmaskService :: [SettingsSubmissionService] -> SettingsSubmissionService -> Either AppError SettingsSubmissionService
unmaskService currentServices service =
  (`withHeaders` service) <$> unmaskSecretsFor targetChanged label storedHeaders service.request.headers
  where
    mStored = L.find ((== service.sId) . (.sId)) currentServices
    storedHeaders = maybe M.empty (.request.headers) mStored
    targetChanged = maybe False ((/= service.request.url) . (.request.url)) mStored
    label = "request url of the submission service " ++ service.sId

withHeaders :: M.Map String String -> SettingsSubmissionService -> SettingsSubmissionService
withHeaders headers service = service {request = service.request {headers = headers}}

maskRegistry :: SettingsRegistry -> SettingsRegistry
maskRegistry settings = settings {apiKey = maskSecret settings.apiKey}

unmaskRegistry :: SettingsRegistry -> SettingsRegistry -> Either AppError SettingsRegistry
unmaskRegistry current settings = Right settings {apiKey = unmaskSecret current.apiKey settings.apiKey}
