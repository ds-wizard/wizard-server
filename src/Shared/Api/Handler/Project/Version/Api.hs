module Shared.Api.Handler.Project.Version.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.Version.Detail_DELETE
import Shared.Api.Handler.Project.Version.Detail_PUT
import Shared.Api.Handler.Project.Version.List_GET
import Shared.Api.Handler.Project.Version.List_POST
import Shared.Api.Handler.WizardCommon

type VersionAPI =
  Tags "Project Version"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_PUT
           :<|> Detail_DELETE
       )

versionApi :: Proxy VersionAPI
versionApi = Proxy

versionServer :: WizardHandlerC s sm r rm => ServerT VersionAPI sm
versionServer =
  list_GET
    :<|> list_POST
    :<|> detail_PUT
    :<|> detail_DELETE
