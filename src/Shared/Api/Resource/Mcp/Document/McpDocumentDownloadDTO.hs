module Shared.Api.Resource.Mcp.Document.McpDocumentDownloadDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics
import GHC.Int

data McpDocumentDownloadDTO = McpDocumentDownloadDTO
  { documentUuid :: U.UUID
  , name :: String
  , contentType :: String
  , fileSize :: Maybe Int64
  , url :: String
  , expiresAt :: UTCTime
  , content :: Maybe String
  , contentLength :: Maybe Int
  }
  deriving (Show, Eq, Generic)
