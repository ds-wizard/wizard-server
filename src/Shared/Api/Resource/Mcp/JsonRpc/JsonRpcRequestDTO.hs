module Shared.Api.Resource.Mcp.JsonRpc.JsonRpcRequestDTO where

import Data.Aeson (Value)
import GHC.Generics

data JsonRpcRequestDTO = JsonRpcRequestDTO
  { id :: Maybe Value
  , method :: String
  , params :: Value
  }
  deriving (Show, Eq, Generic)
