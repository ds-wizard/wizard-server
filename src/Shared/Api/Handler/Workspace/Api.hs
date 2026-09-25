module Shared.Api.Handler.Workspace.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import Shared.Api.Handler.Workspace.Detail_DELETE
import Shared.Api.Handler.Workspace.Detail_GET
import Shared.Api.Handler.Workspace.Detail_PUT
import Shared.Api.Handler.Workspace.List_GET
import Shared.Api.Handler.Workspace.List_POST
import Shared.Api.Handler.Workspace.Member_DELETE
import Shared.Api.Handler.Workspace.Member_PUT
import Shared.Api.Handler.Workspace.Members_GET

type WorkspaceAPI =
  Tags "Workspace"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Members_GET
           :<|> Member_PUT
           :<|> Member_DELETE
       )

workspaceApi :: Proxy WorkspaceAPI
workspaceApi = Proxy

workspaceServer :: WizardHandlerC s sm r rm => ServerT WorkspaceAPI sm
workspaceServer =
  list_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> members_GET
    :<|> member_PUT
    :<|> member_DELETE
