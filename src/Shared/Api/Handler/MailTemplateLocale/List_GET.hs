module Shared.Api.Handler.MailTemplateLocale.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "mail-template-locales"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [MailTemplateLocaleDTO])

list_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] [MailTemplateLocaleDTO])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getMailTemplateLocales
