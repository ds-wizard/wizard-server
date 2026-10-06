module Shared.Integration.Http.Submission.RequestMapper (
  toUploadDocumentRequest,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict as M
import Prelude hiding (lookup)

import Shared.Model.Http.HttpRequest
import Shared.Model.Settings.Settings
import Shared.Util.Interpolation (interpolateMapValues, interpolateString)

toUploadDocumentRequest :: SettingsSubmissionServiceRequest -> M.Map String String -> BS.ByteString -> HttpRequest
toUploadDocumentRequest req variables reqBody =
  HttpRequest
    { requestMethod = req.method
    , requestUrl = interpolateString variables req.url
    , requestHeaders = interpolateMapValues variables req.headers
    , requestBody = reqBody
    , multipart =
        if req.multipart.enabled
          then Just HttpRequestMultipart {key = req.multipart.fileName, fileName = Nothing, contentType = Nothing}
          else Nothing
    }
