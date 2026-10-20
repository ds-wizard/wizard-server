module Shared.Api.Resource.Mcp.Toolset.McpToolsetJM where

import Data.Aeson
import qualified Data.Text as T
import Servant.API

import Shared.Model.Mcp.McpToolset
import Shared.Util.String (splitOn)

instance ToJSON McpToolset where
  toJSON = String . T.pack . mcpToolsetName

instance FromJSON McpToolset where
  parseJSON = withText "McpToolset" $ either (fail . T.unpack) return . parseToolset . T.unpack

instance FromHttpApiData [McpToolset] where
  parseQueryParam = traverse parseToolset . splitOn "," . T.unpack

parseToolset :: String -> Either T.Text McpToolset
parseToolset name = maybe (Left . T.pack $ "Unknown MCP toolset: " ++ name) Right (parseMcpToolset name)
