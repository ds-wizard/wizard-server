module Shared.Api.Resource.Mcp.Code.McpCodeJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Code.McpCodeDTO

instance ToJSON McpCodeDTO where
  toJSON McpCodeDTO {..} =
    object
      [ "code" .= code
      ]
