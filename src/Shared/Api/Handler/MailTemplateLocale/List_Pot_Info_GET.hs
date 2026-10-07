module Shared.Api.Handler.MailTemplateLocale.List_Pot_Info_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService

type List_Pot_Info_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "mail-template-locales"
    :> "pot"
    :> "info"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] MailTemplatePotInfoDTO)

list_pot_info_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> sm (Headers '[Header "x-trace-uuid" String] MailTemplatePotInfoDTO)
list_pot_info_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getMailTemplatePotInfo
