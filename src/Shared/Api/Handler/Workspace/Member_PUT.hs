module Shared.Api.Handler.Workspace.Member_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Model.Context.TransactionState
import Shared.Service.Workspace.WorkspaceService

type Member_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> "workspaces"
    :> Capture "uuid" U.UUID
    :> "members"
    :> Capture "userUuid" U.UUID
    :> Verb 'PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

member_PUT :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> U.UUID -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
member_PUT mTokenHeader mServerUrl uuid userUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        addWorkspaceMember uuid userUuid
        return NoContent
