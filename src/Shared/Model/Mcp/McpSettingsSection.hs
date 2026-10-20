module Shared.Model.Mcp.McpSettingsSection where

import Data.Aeson (Object, Value)

data McpSettingsSection m = McpSettingsSection
  { name :: String
  , valueSchema :: Value
  , sensitive :: Bool
  , sensitiveRead :: Bool
  , getSection :: m Value
  , updateSection :: Object -> Maybe Bool -> m Value
  , resetOverride :: Maybe (m ())
  }
