module Shared.Api.Resource.Mcp.Common.McpNonNegativeArgumentJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.Common.McpNonNegativeArgumentDTO
import Shared.Util.JsonSchema

instance FromJSON McpNonNegativeArgumentDTO where
  parseJSON value = do
    number <- parseJSON value
    if number < 0 then fail "must not be negative" else return (McpNonNegativeArgumentDTO number)

instance JsonSchema McpNonNegativeArgumentDTO where
  jsonSchema _ = object ["type" .= ("integer" :: String), "minimum" .= (0 :: Int)]
