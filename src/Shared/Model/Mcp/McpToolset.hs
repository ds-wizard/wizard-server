module Shared.Model.Mcp.McpToolset where

import GHC.Generics

data McpToolset
  = KmMcpToolset
  | DtMcpToolset
  | ProjectsMcpToolset
  | AdminMcpToolset
  | AnalyticsMcpToolset
  deriving (Show, Eq, Enum, Bounded, Generic)

allMcpToolsets :: [McpToolset]
allMcpToolsets = [minBound .. maxBound]

mcpToolsetName :: McpToolset -> String
mcpToolsetName KmMcpToolset = "km"
mcpToolsetName DtMcpToolset = "dt"
mcpToolsetName ProjectsMcpToolset = "projects"
mcpToolsetName AdminMcpToolset = "admin"
mcpToolsetName AnalyticsMcpToolset = "analytics"

parseMcpToolset :: String -> Maybe McpToolset
parseMcpToolset name = lookup name [(mcpToolsetName toolset, toolset) | toolset <- allMcpToolsets]
