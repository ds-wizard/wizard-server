module Shared.Api.Handler.MailTemplateLocale.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Api.Resource.Locale.LocaleChangeJM ()
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] LocaleChangeDTO
    :> "mail-template-locales"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] MailTemplateLocaleDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> LocaleChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] MailTemplateLocaleDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyMailTemplateLocale uuid reqDto
