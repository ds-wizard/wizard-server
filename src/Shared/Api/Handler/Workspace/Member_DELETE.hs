module Shared.Api.Handler.Workspace.Member_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.Workspace.WorkspaceService

type Member_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "workspaces"
    :> Capture "uuid" U.UUID
    :> "members"
    :> Capture "userUuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

member_DELETE :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
member_DELETE mTokenHeader mServerUrl uuid userUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        removeWorkspaceMember uuid userUuid
        return NoContent
