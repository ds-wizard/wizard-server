module Shared.Api.Handler.DocumentTemplate.Detail_Locales_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateDTO
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleCreateJM ()
import Shared.Api.Resource.DocumentTemplate.Locale.DocumentTemplateLocaleListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.DocumentTemplate.Locale.DocumentTemplateLocaleList
import Shared.Service.DocumentTemplate.Locale.DocumentTemplateLocaleService

type Detail_Locales_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem DocumentTemplateLocaleCreateDTO
    :> "document-templates"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] DocumentTemplateLocaleList)

detail_locales_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> DocumentTemplateLocaleCreateDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] DocumentTemplateLocaleList)
detail_locales_POST mTokenHeader mServerUrl reqDto dtUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createLocale dtUuid reqDto
