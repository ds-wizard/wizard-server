module Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateDTO where

import GHC.Generics

data McpClientRegistrationCreateDTO = McpClientRegistrationCreateDTO
  { redirectUris :: [String]
  , clientName :: Maybe String
  , tokenEndpointAuthMethod :: Maybe String
  }
  deriving (Show, Eq, Generic)
