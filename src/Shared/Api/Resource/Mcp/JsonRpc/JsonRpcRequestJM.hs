module Shared.Api.Resource.Mcp.JsonRpc.JsonRpcRequestJM where

import Data.Aeson

import Shared.Api.Resource.Mcp.JsonRpc.JsonRpcRequestDTO

instance FromJSON JsonRpcRequestDTO where
  parseJSON = withObject "JsonRpcRequestDTO" $ \o -> do
    version <- o .: "jsonrpc"
    if version /= ("2.0" :: String)
      then fail "jsonrpc must be 2.0"
      else
        JsonRpcRequestDTO
          <$> o .:! "id"
          <*> o .: "method"
          <*> o .:? "params" .!= object []

instance ToJSON JsonRpcRequestDTO where
  toJSON JsonRpcRequestDTO {..} =
    object
      [ "jsonrpc" .= ("2.0" :: String)
      , "id" .= id
      , "method" .= method
      , "params" .= params
      ]
