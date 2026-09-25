module Shared.Api.Handler.Workspace.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Workspace.Workspace
import Shared.Service.Workspace.WorkspaceService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "workspaces"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page Workspace))

list_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page Workspace))
list_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getWorkspacesPage mQuery (Pageable mPage mSize) (parseSortQuery mSort)
