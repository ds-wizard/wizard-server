module Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Registration.McpClientRegistrationCreateDTO

instance FromJSON McpClientRegistrationCreateDTO where
  parseJSON = withObject "McpClientRegistrationCreateDTO" $ \o ->
    McpClientRegistrationCreateDTO
      <$> o .: "redirect_uris"
      <*> o .:? "client_name"
      <*> o .:? "token_endpoint_auth_method"

instance ToJSON McpClientRegistrationCreateDTO where
  toJSON McpClientRegistrationCreateDTO {..} =
    object
      [ "redirect_uris" .= redirectUris
      , "client_name" .= clientName
      , "token_endpoint_auth_method" .= tokenEndpointAuthMethod
      ]
