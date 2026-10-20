module Shared.Api.Resource.Mcp.DocumentPreview.McpDocumentPreviewJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.DocumentPreview.McpDocumentPreviewDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema
import Shared.Util.String (lowerFirst, stripSuffixIfExists)

instance FromJSON McpDocumentPreviewState where
  parseJSON = genericParseJSON previewStateOptions

instance ToJSON McpDocumentPreviewState where
  toJSON = genericToJSON previewStateOptions

instance JsonSchema McpDocumentPreviewState where
  jsonSchema = genericJsonSchema previewStateOptions

instance FromJSON McpDocumentPreviewDTO where
  parseJSON = genericParseJSON previewOptions

instance ToJSON McpDocumentPreviewDTO where
  toJSON = genericToJSON previewOptions

instance JsonSchema McpDocumentPreviewDTO where
  jsonSchema = genericJsonSchema previewOptions

previewStateOptions :: Options
previewStateOptions = jsonOptions {constructorTagModifier = lowerFirst . stripSuffixIfExists "McpDocumentPreviewState"}

previewOptions :: Options
previewOptions = jsonOptions {omitNothingFields = True}
