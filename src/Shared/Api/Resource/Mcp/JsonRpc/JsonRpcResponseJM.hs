module Shared.Api.Resource.Mcp.JsonRpc.JsonRpcResponseJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcResponseDTO

instance ToJSON JsonRpcResponseDTO where
  toJSON JsonRpcResponseDTO {..} =
    object $ ["jsonrpc" .= ("2.0" :: String), "id" .= id] ++ either (\e -> ["error" .= e]) (\r -> ["result" .= r]) outcome

instance ToJSON JsonRpcErrorDTO where
  toJSON JsonRpcErrorDTO {..} = object ["code" .= code, "message" .= message]
