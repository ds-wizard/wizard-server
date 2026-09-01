module Shared.Api.Handler.KnowledgeModelPackage.Dependent.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpactJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> Capture "uuid" U.UUID
    :> "dependents"
    :> QueryParam "allVersions" Bool
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageDeletionImpact])

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe Bool
  -> sm (Headers '[Header "x-trace-uuid" String] [KnowledgeModelPackageDeletionImpact])
list_GET mTokenHeader mServerUrl uuid mAllVersions =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getDependentPackageResources uuid mAllVersions
