module Shared.Api.Resource.Mcp.DocumentPreview.McpDocumentPreviewDTO where

import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data McpDocumentPreviewState
  = PendingMcpDocumentPreviewState
  | DoneMcpDocumentPreviewState
  | ErrorMcpDocumentPreviewState
  deriving (Show, Eq, Generic)

data McpDocumentPreviewDTO = McpDocumentPreviewDTO
  { state :: McpDocumentPreviewState
  , documentUuid :: U.UUID
  , fileName :: Maybe String
  , contentType :: Maybe String
  , fileSize :: Maybe Int64
  , content :: Maybe String
  , contentLength :: Maybe Int
  , errorMessage :: Maybe String
  , workerLog :: Maybe String
  }
  deriving (Show, Eq, Generic)
