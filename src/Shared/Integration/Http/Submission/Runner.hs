module Shared.Integration.Http.Submission.Runner (
  uploadDocument,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict as M

import Shared.Integration.Http.Common.HttpClient
import Shared.Integration.Http.Submission.RequestMapper
import Shared.Integration.Http.Submission.ResponseMapper
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Settings.Settings

uploadDocument
  :: WizardRequestContextC s m
  => SettingsSubmissionServiceRequest
  -> M.Map String String
  -> BS.ByteString
  -> m (Either String (Maybe String))
uploadDocument reqTemplate variables reqBody =
  runRequest' (toUploadDocumentRequest reqTemplate variables reqBody) toUploadDocumentResponse
