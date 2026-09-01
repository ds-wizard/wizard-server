module Shared.Api.Handler.KnowledgeModelPackage.Detail_Locales_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateDTO
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleCreateJM ()
import Shared.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import Shared.Service.KnowledgeModel.Locale.KnowledgeModelLocaleService

type Detail_Locales_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem KnowledgeModelLocaleCreateDTO
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> "locales"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelLocaleList)

detail_locales_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelLocaleCreateDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelLocaleList)
detail_locales_POST mTokenHeader mServerUrl reqDto pkgUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createLocale pkgUuid reqDto
