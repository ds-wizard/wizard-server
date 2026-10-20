module Shared.Api.Handler.Mcp.Detail_Token_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.Mcp.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Mcp.Token.McpTokenCreateDTO
import Shared.Api.Resource.Mcp.Token.McpTokenCreateJM ()
import Shared.Api.Resource.Mcp.Token.McpTokenDTO
import Shared.Api.Resource.Mcp.Token.McpTokenJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Mcp.McpFlowService

type Detail_Token_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[FormUrlEncoded] McpTokenCreateDTO
    :> "mcp"
    :> "oauth2"
    :> "token"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] McpTokenDTO)

detail_token_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> McpTokenCreateDTO
  -> sm (Headers '[Header "x-trace-uuid" String] McpTokenDTO)
detail_token_POST mAuthHeader mServerUrl reqDto =
  respondOAuthWith (basicChallenge mAuthHeader) . runInUnauthService mServerUrl Transactional $
    traverse addTraceUuidHeader =<< exchangeMcpToken mAuthHeader reqDto
