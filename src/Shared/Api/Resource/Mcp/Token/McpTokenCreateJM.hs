module Shared.Api.Resource.Mcp.Token.McpTokenCreateJM where

import Web.FormUrlEncoded

import Shared.Api.Resource.Mcp.Token.McpTokenCreateDTO

instance FromForm McpTokenCreateDTO where
  fromForm f =
    McpTokenCreateDTO
      <$> parseMaybe "grant_type" f
      <*> parseMaybe "code" f
      <*> parseMaybe "client_id" f
      <*> parseMaybe "client_secret" f
      <*> parseMaybe "redirect_uri" f
      <*> parseMaybe "code_verifier" f
      <*> parseMaybe "refresh_token" f
