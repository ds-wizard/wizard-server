module Shared.Api.Handler.Workspace.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceChangeJM ()
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Workspace.Workspace
import Shared.Service.Workspace.WorkspaceService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] WorkspaceChangeDTO
    :> "workspaces"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] Workspace)

detail_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> WorkspaceChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] Workspace)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyWorkspace uuid reqDto
