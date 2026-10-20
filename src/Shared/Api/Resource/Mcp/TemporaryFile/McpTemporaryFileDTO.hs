module Shared.Api.Resource.Mcp.TemporaryFile.McpTemporaryFileDTO where

import Data.Time
import GHC.Generics

data McpTemporaryFileDTO = McpTemporaryFileDTO
  { url :: String
  , contentType :: String
  , expiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
