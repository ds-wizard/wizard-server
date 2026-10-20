module Shared.Api.Handler.Mcp.List_POST where

import qualified Data.Aeson as A
import Network.Wai (Response, ResponseReceived)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.Mcp.Common
import Shared.Api.Handler.Mcp.McpPost
import Shared.Api.Handler.Mcp.McpStream
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Toolset.McpToolsetJM ()
import Shared.Api.Resource.UserToken.UserTokenClaimsDTO
import Shared.Model.Context.TransactionState
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Service.Mcp.Server.McpServerService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] A.Value
    :> "mcp"
    :> QueryParam "toolsets" [McpToolset]
    :> Description "MCP endpoint (JSON-RPC 2.0, stateless): initialize, ping, tools/list, tools/call; a notification answers 202, a tools/call with a progressToken may answer text/event-stream with its progress"
    :> McpPost

list_POST
  :: WizardHandlerC s sm r rm
  => [McpTool rm]
  -> Maybe String
  -> Maybe String
  -> A.Value
  -> Maybe [McpToolset]
  -> (Response -> IO ResponseReceived)
  -> sm ResponseReceived
list_POST tools mTokenHeader mServerUrl reqBody mToolsets respond =
  withResourceMetadata mServerUrl $
    getMcpAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService -> do
      mTokenUuid <- traverse (fmap (.tokenUuid) . validateJwtToken) mTokenHeader
      mResponded <- runInAuthService NoTransaction $ respondMcp respond reqBody (handleMcpRequest tools mTokenUuid mToolsets reqBody)
      maybe (throwError mcpAccepted) return mResponded
