module Shared.Api.Handler.KnowledgeModelPackage.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-packages"
    :> QueryParam "w" U.UUID
    :> QueryParam "tenant" Bool
    :> QueryParam "id" String
    :> QueryParam "q" String
    :> QueryParam "outdated" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSimpleDTO))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelPackageSimpleDTO))
list_GET mTokenHeader mServerUrl mW mTenant mId mQuery mOutdated mPage mSize mSort =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW mTenant $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getPackagesPage mId mQuery mOutdated (Pageable mPage mSize) (parseSortQuery mSort)
