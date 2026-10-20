module Shared.Api.Resource.Mcp.Error.McpOAuthErrorJM where

import Data.Aeson

import Shared.Model.Mcp.McpOAuthError

instance ToJSON McpOAuthError where
  toJSON McpOAuthError {..} =
    object
      [ "error" .= errorCode
      , "error_description" .= errorDescription
      ]
