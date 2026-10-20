module Shared.Api.Resource.Mcp.Registration.McpClientRegistrationJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationDTO

instance ToJSON McpClientRegistrationDTO where
  toJSON McpClientRegistrationDTO {..} =
    object $
      [ "client_id" .= clientId
      , "client_id_issued_at" .= clientIdIssuedAt
      , "client_secret_expires_at" .= clientSecretExpiresAt
      , "redirect_uris" .= redirectUris
      , "client_name" .= clientName
      , "token_endpoint_auth_method" .= tokenEndpointAuthMethod
      ]
        ++ maybe [] (\secret -> ["client_secret" .= secret]) clientSecret
