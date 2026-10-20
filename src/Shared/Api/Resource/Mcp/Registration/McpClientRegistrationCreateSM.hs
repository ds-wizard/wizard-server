module Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateSM where

import Data.Swagger

import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateDTO
import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateJM ()

instance ToSchema McpClientRegistrationCreateDTO where
  declareNamedSchema _ = return . NamedSchema (Just "McpClientRegistrationCreateDTO") $ mempty
