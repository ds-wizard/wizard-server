module Shared.Api.Handler.Mcp.Detail_Tokens_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.UserToken.UserTokenListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.User.UserToken
import Shared.Model.User.UserTokenList
import Shared.Service.UserToken.UserTokenService

type Detail_Tokens_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "mcp"
    :> "tokens"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [UserTokenList])

detail_tokens_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] [UserTokenList])
detail_tokens_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getTokens McpUserTokenType Nothing
