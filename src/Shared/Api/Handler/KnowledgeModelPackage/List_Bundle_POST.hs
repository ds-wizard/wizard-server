module Shared.Api.Handler.KnowledgeModelPackage.List_Bundle_POST where

import qualified Data.UUID as U
import Servant
import Servant.Multipart

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleFileJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundleFile
import Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService

type List_Bundle_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> MultipartForm Mem KnowledgeModelBundleFile
    :> "knowledge-model-packages"
    :> "bundle"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

list_bundle_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelBundleFile
  -> Maybe U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
list_bundle_POST mTokenHeader mServerUrl reqDto mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< importAndConvertBundle reqDto.content False
