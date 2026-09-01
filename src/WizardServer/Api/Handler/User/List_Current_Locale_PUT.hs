module WizardServer.Api.Handler.User.List_Current_Locale_PUT where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.User.UserLocaleDTO
import Shared.Model.Context.TransactionState
import Shared.Service.User.Profile.UserProfileService

type List_Current_Locale_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] UserLocaleDTO
    :> "users"
    :> "current"
    :> "locale"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] UserLocaleDTO)

list_current_locale_PUT :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> UserLocaleDTO -> sm (Headers '[Header "x-trace-uuid" String] UserLocaleDTO)
list_current_locale_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< modifyLocale reqDto
