module Shared.Api.Resource.Mcp.JsonRpc.JsonRpcResponseDTO where

import Data.Aeson (Value)
import GHC.Generics

data JsonRpcResponseDTO = JsonRpcResponseDTO
  { id :: Value
  , outcome :: Either JsonRpcErrorDTO Value
  }
  deriving (Show, Eq, Generic)

data JsonRpcErrorDTO = JsonRpcErrorDTO
  { code :: Int
  , message :: String
  }
  deriving (Show, Eq, Generic)
