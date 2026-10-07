module Shared.Api.Handler.MailTemplateLocale.Detail_Po_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.TemporaryFile.TemporaryFileDTO
import Shared.Api.Resource.TemporaryFile.TemporaryFileJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.MailTemplateLocale.MailTemplateLocaleService

type Detail_Po_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "mail-template-locales"
    :> Capture "uuid" U.UUID
    :> "po"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)

detail_po_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] TemporaryFileDTO)
detail_po_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getMailTemplateLocalePo uuid
