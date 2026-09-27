module Shared.Api.Handler.Workspace.Detail_Members_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeDTO
import Shared.Api.Resource.Workspace.WorkspaceMemberChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Workspace.WorkspaceService

type Detail_Members_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] WorkspaceMemberChangeDTO
    :> "workspaces"
    :> Capture "uuid" U.UUID
    :> "members"
    :> Capture "userUuid" U.UUID
    :> Verb 'PUT 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_members_PUT :: WizardHandlerC s sm r rm => Maybe String -> Maybe String -> WorkspaceMemberChangeDTO -> U.UUID -> U.UUID -> sm (Headers '[Header "x-trace-uuid" String] NoContent)
detail_members_PUT mTokenHeader mServerUrl reqDto uuid userUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        addWorkspaceMember uuid userUuid reqDto
        return NoContent
