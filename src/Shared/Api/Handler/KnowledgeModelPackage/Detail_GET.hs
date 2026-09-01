module Shared.Api.Handler.KnowledgeModelPackage.Detail_GET where

import Data.Maybe (fromMaybe)
import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> QueryParam "excludeDeprecatedVersions" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> Maybe Bool -> sm (Headers '[Header "x-trace-uuid" String] KnowledgeModelPackageDetailDTO)
detail_GET mTokenHeader mServerUrl pkgUuid mExcludeDeprecatedVersions =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $ addTraceUuidHeader =<< getPackageDetailByUuid pkgUuid (fromMaybe False mExcludeDeprecatedVersions)
