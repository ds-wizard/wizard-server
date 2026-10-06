module Shared.Api.Handler.Bootstrap.List_Workspace_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Bootstrap.BootstrapJM ()
import Shared.Api.Resource.Bootstrap.WorkspaceBootstrapDTO
import Shared.Model.Context.TransactionState
import Shared.Service.Bootstrap.BootstrapService

type List_Workspace_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "bootstrap"
    :> "workspace"
    :> QueryParam "w" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] WorkspaceBootstrapDTO)

list_workspace_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> Maybe U.UUID -> sm (Headers '[Header "x-trace-uuid" String] WorkspaceBootstrapDTO)
list_workspace_GET mTokenHeader mServerUrl mW =
  getScopedAuthServiceExecutor mTokenHeader mServerUrl mW Nothing $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getWorkspaceBootstrap
