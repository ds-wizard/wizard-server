module Shared.Api.Handler.Project.File.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.File.Detail_DELETE
import Shared.Api.Handler.Project.File.Detail_Download_GET
import Shared.Api.Handler.Project.File.List_GET
import Shared.Api.Handler.Project.File.List_POST
import Shared.Api.Handler.WizardCommon

type FileAPI =
  Tags "Project File"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_DELETE
           :<|> Detail_Download_GET
       )

fileApi :: Proxy FileAPI
fileApi = Proxy

fileServer :: WizardHandlerC s sm r rm => ServerT FileAPI sm
fileServer =
  list_GET
    :<|> list_POST
    :<|> detail_DELETE
    :<|> detail_download_GET
