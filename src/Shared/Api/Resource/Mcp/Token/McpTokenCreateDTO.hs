module Shared.Api.Resource.Mcp.Token.McpTokenCreateDTO where

import GHC.Generics

data McpTokenCreateDTO = McpTokenCreateDTO
  { grantType :: Maybe String
  , code :: Maybe String
  , clientId :: Maybe String
  , clientSecret :: Maybe String
  , redirectUri :: Maybe String
  , codeVerifier :: Maybe String
  , refreshToken :: Maybe String
  }
  deriving (Show, Eq, Generic)
