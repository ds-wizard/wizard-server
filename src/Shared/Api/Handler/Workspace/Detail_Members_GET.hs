module Shared.Api.Handler.Workspace.Detail_Members_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Workspace.WorkspaceMemberJM ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Context.TransactionState
import Shared.Model.Workspace.WorkspaceMember
import Shared.Service.Workspace.WorkspaceService

type Detail_Members_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "workspaces"
    :> Capture "uuid" U.UUID
    :> "members"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page WorkspaceMember))

detail_members_GET
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> sm (Headers '[Header "x-trace-uuid" String] (Page WorkspaceMember))
detail_members_GET mTokenHeader mServerUrl uuid mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getWorkspaceMembersPage uuid mQuery (Pageable mPage mSize) (parseSortQuery mSort)
