module Shared.Api.Resource.Mcp.Plugin.McpPluginClient where

import Data.Swagger
import qualified Data.Text as T
import GHC.Generics
import Servant.API

data McpPluginClient
  = ClaudeMcpPluginClient
  | ChatGptMcpPluginClient
  deriving (Show, Eq, Generic)

instance FromHttpApiData McpPluginClient where
  parseUrlPiece "claude" = Right ClaudeMcpPluginClient
  parseUrlPiece "chatgpt" = Right ChatGptMcpPluginClient
  parseUrlPiece client = Left $ T.append "Unknown MCP plugin client: " client

instance ToParamSchema McpPluginClient where
  toParamSchema _ =
    mempty
      { _paramSchemaType = Just SwaggerString
      , _paramSchemaEnum = Just ["claude", "chatgpt"]
      }
