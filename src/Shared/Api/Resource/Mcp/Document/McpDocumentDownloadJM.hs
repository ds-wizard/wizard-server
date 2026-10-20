module Shared.Api.Resource.Mcp.Document.McpDocumentDownloadJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Document.McpDocumentDownloadDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON McpDocumentDownloadDTO where
  parseJSON = genericParseJSON downloadOptions

instance ToJSON McpDocumentDownloadDTO where
  toJSON = genericToJSON downloadOptions

instance JsonSchema McpDocumentDownloadDTO where
  jsonSchema = genericJsonSchema downloadOptions

downloadOptions :: Options
downloadOptions = jsonOptions {omitNothingFields = True}
