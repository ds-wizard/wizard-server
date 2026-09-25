module Shared.Api.Handler.Workspace.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Workspace.Workspace
import Shared.Service.Workspace.WorkspaceService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "workspaces"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] Workspace)

detail_GET :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] Workspace)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getWorkspaceByUuid uuid
