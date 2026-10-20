module Shared.Api.Resource.Mcp.Metadata.McpMetadataDTO where

import GHC.Generics

data McpMetadataDTO = McpMetadataDTO
  { issuer :: String
  , authorizationEndpoint :: String
  , tokenEndpoint :: String
  , registrationEndpoint :: String
  , jwksUri :: String
  , responseTypesSupported :: [String]
  , responseModesSupported :: [String]
  , grantTypesSupported :: [String]
  , tokenEndpointAuthMethodsSupported :: [String]
  , codeChallengeMethodsSupported :: [String]
  , scopesSupported :: [String]
  , subjectTypesSupported :: [String]
  , idTokenSigningAlgValuesSupported :: [String]
  }
  deriving (Show, Eq, Generic)
