module Shared.Api.Handler.Workspace.List_POST where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Workspace.WorkspaceChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceChangeJM ()
import Shared.Api.Resource.Workspace.WorkspaceJM ()
import Shared.Model.Context.TransactionState
import Shared.Model.Workspace.Workspace
import Shared.Service.Workspace.WorkspaceService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] WorkspaceChangeDTO
    :> "workspaces"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] Workspace)

list_POST
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> WorkspaceChangeDTO
  -> sm (Headers '[Header "x-trace-uuid" String] Workspace)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createWorkspace reqDto
