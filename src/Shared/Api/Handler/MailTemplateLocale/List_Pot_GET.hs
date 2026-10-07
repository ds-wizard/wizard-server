module Shared.Api.Handler.MailTemplateLocale.List_Pot_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService

type List_Pot_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "mail-template-locales"
    :> "pot"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

list_pot_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
list_pot_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getMailTemplatePot
