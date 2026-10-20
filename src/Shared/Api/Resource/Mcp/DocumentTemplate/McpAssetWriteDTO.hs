module Shared.Api.Resource.Mcp.DocumentTemplate.McpAssetWriteDTO where

import qualified Data.ByteString.Char8 as BS

data McpAssetWriteDTO = McpAssetWriteDTO
  { path :: String
  , content :: Maybe BS.ByteString
  , contentType :: Maybe String
  , newPath :: Maybe String
  }
