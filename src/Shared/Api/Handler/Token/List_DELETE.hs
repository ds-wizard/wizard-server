module Shared.Api.Handler.Token.List_DELETE where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Localization.Messages.Public
import Shared.Model.Cache.ServerCache
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Service.UserToken.UserTokenService
import Shared.Util.Token

type List_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "tokens"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_DELETE :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
list_DELETE mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        let forbid = throwError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
        case mTokenHeader of
          Just tokenHeader ->
            case separateToken tokenHeader of
              Just token -> do
                deleteTokensExceptCurrentSession token
                return NoContent
              _ -> forbid
          _ -> forbid
