module Shared.Api.Resource.Mcp.DocumentTemplate.McpFileWriteDTO where

data McpFileWriteDTO = McpFileWriteDTO
  { path :: String
  , content :: Maybe String
  , newPath :: Maybe String
  }
