module Shared.Api.Resource.Mcp.Metadata.McpMetadataJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Metadata.McpMetadataDTO

instance ToJSON McpMetadataDTO where
  toJSON McpMetadataDTO {..} =
    object
      [ "issuer" .= issuer
      , "authorization_endpoint" .= authorizationEndpoint
      , "token_endpoint" .= tokenEndpoint
      , "registration_endpoint" .= registrationEndpoint
      , "jwks_uri" .= jwksUri
      , "response_types_supported" .= responseTypesSupported
      , "response_modes_supported" .= responseModesSupported
      , "grant_types_supported" .= grantTypesSupported
      , "token_endpoint_auth_methods_supported" .= tokenEndpointAuthMethodsSupported
      , "code_challenge_methods_supported" .= codeChallengeMethodsSupported
      , "scopes_supported" .= scopesSupported
      , "subject_types_supported" .= subjectTypesSupported
      , "id_token_signing_alg_values_supported" .= idTokenSigningAlgValuesSupported
      ]
