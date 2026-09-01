module Shared.Integration.Http.Submission.Runner (
  uploadDocument,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict as M

import Shared.Integration.Http.Common.HttpClient
import Shared.Integration.Http.Submission.RequestMapper
import Shared.Integration.Http.Submission.ResponseMapper
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig

uploadDocument
  :: WizardRequestContextC s m
  => TenantConfigSubmissionServiceRequest
  -> M.Map String String
  -> BS.ByteString
  -> m (Either String (Maybe String))
uploadDocument reqTemplate variables reqBody =
  runRequest' (toUploadDocumentRequest reqTemplate variables reqBody) toUploadDocumentResponse
