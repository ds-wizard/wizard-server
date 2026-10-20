module Shared.Api.Middleware.McpRequestSizeMiddleware where

import qualified Data.ByteString.Char8 as BS
import Network.Wai
import Network.Wai.Middleware.RequestSizeLimit

import Shared.Constant.Api
import Shared.Constant.Mcp

mcpRequestSizeMiddleware :: Middleware
mcpRequestSizeMiddleware = requestSizeLimitMiddleware (setMaxLengthForRequest maxLength defaultRequestSizeLimitSettings)
  where
    maxLength req
      | rawPathInfo req == BS.pack (apiPrefix ++ "/mcp") = return (Just mcpMaxRequestSize)
      | otherwise = return Nothing
