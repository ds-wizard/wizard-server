module Shared.Api.Resource.Mcp.Code.McpCodeCreateJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Code.McpCodeCreateDTO
import Shared.Api.Resource.Mcp.Toolset.McpToolsetJM ()

instance FromJSON McpCodeCreateDTO where
  parseJSON = withObject "McpCodeCreateDTO" $ \o ->
    McpCodeCreateDTO
      <$> o .: "clientId"
      <*> o .: "redirectUri"
      <*> o .:? "codeChallenge"
      <*> o .:? "codeChallengeMethod"
      <*> o .:? "toolsets"

instance ToJSON McpCodeCreateDTO where
  toJSON McpCodeCreateDTO {..} =
    object
      [ "clientId" .= clientId
      , "redirectUri" .= redirectUri
      , "codeChallenge" .= codeChallenge
      , "codeChallengeMethod" .= codeChallengeMethod
      , "toolsets" .= toolsets
      ]
