module Shared.Api.Handler.KnowledgeModelPackage.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelPackageChangeDTO
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageChangeDTO)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> KnowledgeModelPackageChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageChangeDTO)
detail_PUT mTokenHeader mServerUrl reqDto pkgUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyPackage pkgUuid reqDto
