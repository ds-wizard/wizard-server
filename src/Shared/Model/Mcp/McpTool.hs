module Shared.Model.Mcp.McpTool where

import Data.Aeson (Value)
import Data.List (intercalate)

import Shared.Model.Context.TransactionState
import Shared.Model.Mcp.McpToolset
import Shared.Util.String (splitOn)

data McpTool m = McpTool
  { name :: String
  , title :: String
  , description :: String
  , inputSchema :: Value
  , outputSchema :: Maybe Value
  , annotations :: McpToolAnnotations
  , continuable :: Bool
  , permissions :: [String]
  , toolset :: Maybe McpToolset
  , transactionState :: TransactionState
  , run :: Value -> Either String (m Value)
  }

data McpToolAnnotations = McpToolAnnotations
  { readOnlyHint :: Bool
  , destructiveHint :: Bool
  , idempotentHint :: Bool
  , openWorldHint :: Bool
  }

anyOfPermissions :: [String] -> String
anyOfPermissions = intercalate " or "

permissionAlternatives :: String -> [String]
permissionAlternatives = splitOn " or "
