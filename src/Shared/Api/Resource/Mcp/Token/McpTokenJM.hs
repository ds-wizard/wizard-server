module Shared.Api.Resource.Mcp.Token.McpTokenJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Token.McpTokenDTO

instance ToJSON McpTokenDTO where
  toJSON McpTokenDTO {..} =
    object
      [ "access_token" .= accessToken
      , "token_type" .= tokenType
      , "expires_in" .= expiresIn
      , "refresh_token" .= refreshToken
      ]
