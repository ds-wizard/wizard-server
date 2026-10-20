module Shared.Api.Resource.Mcp.Registration.McpClientRegistrationSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationJM ()

instance ToSchema McpClientRegistrationDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpClientRegistrationDTO") $ mempty
