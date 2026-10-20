module Shared.Model.Mcp.McpCursor where

import Data.Aeson
import GHC.Generics

import Shared.Util.Aeson

data McpCursor = McpCursor
  { path :: [McpCursorStep]
  , offset :: Int
  }
  deriving (Show, Eq, Generic)

data McpCursorStep
  = KeyMcpCursorStep String
  | IndexMcpCursorStep Int
  deriving (Show, Eq, Generic)

instance FromJSON McpCursor where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON McpCursor where
  toJSON = genericToJSON jsonOptions

instance FromJSON McpCursorStep where
  parseJSON (String key) = KeyMcpCursorStep <$> parseJSON (String key)
  parseJSON value = IndexMcpCursorStep <$> parseJSON value

instance ToJSON McpCursorStep where
  toJSON (KeyMcpCursorStep key) = toJSON key
  toJSON (IndexMcpCursorStep index) = toJSON index
