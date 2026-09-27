module Shared.Api.Handler.Workspace.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import Shared.Api.Handler.Workspace.Detail_DELETE
import Shared.Api.Handler.Workspace.Detail_GET
import Shared.Api.Handler.Workspace.Detail_Members_DELETE
import Shared.Api.Handler.Workspace.Detail_Members_GET
import Shared.Api.Handler.Workspace.Detail_Members_PUT
import Shared.Api.Handler.Workspace.Detail_PUT
import Shared.Api.Handler.Workspace.List_GET
import Shared.Api.Handler.Workspace.List_POST

type WorkspaceAPI =
  Tags "Workspace"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Detail_Members_GET
           :<|> Detail_Members_PUT
           :<|> Detail_Members_DELETE
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
    :<|> detail_members_GET
    :<|> detail_members_PUT
    :<|> detail_members_DELETE
