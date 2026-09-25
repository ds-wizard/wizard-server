module Shared.Api.Handler.KnowledgeModelPackage.Detail_Pull_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService

type Detail_Pull_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "id" String
    :> "pull"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> Verb POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)

detail_pull_POST :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> String -> Maybe U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageSimpleDTO)
detail_pull_POST mTokenHeader mServerUrl pkgId mW mTenant =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< pullBundleFromRegistry pkgId
