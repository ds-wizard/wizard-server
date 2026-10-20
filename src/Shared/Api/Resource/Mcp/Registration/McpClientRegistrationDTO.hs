module Shared.Api.Resource.Mcp.Registration.McpClientRegistrationDTO where

import GHC.Generics

data McpClientRegistrationDTO = McpClientRegistrationDTO
  { clientId :: String
  , clientSecret :: Maybe String
  , clientIdIssuedAt :: Integer
  , clientSecretExpiresAt :: Integer
  , redirectUris :: [String]
  , clientName :: Maybe String
  , tokenEndpointAuthMethod :: String
  }
  deriving (Show, Eq, Generic)
