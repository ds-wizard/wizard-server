module Shared.Api.Handler.Token.List_System_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Api.Resource.UserToken.UserTokenJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Context.TransactionState
import Shared.Model.Error.Error
import Shared.Service.UserToken.System.SystemService
import Shared.Util.Token

type List_System_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> Header "User-Agent" String
    :> "tokens"
    :> "system"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserTokenDTO)

list_system_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] UserTokenDTO)
list_system_POST mTokenHeader mServerUrl mUserAgent =
  runInUnauthService mServerUrl Transactional $
    addTraceUuidHeader =<< do
      let forbid = throwError . UnauthorizedError $ _ERROR_API_COMMON__UNABLE_TO_GET_TOKEN
      case mTokenHeader of
        Just tokenHeader ->
          case separateToken tokenHeader of
            Just token -> createSystemToken token mUserAgent
            _ -> forbid
        _ -> forbid
