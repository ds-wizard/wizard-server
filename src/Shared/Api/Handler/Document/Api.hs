module Shared.Api.Handler.Document.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Document.Detail_Available_Submission_Services_GET
import Shared.Api.Handler.Document.Detail_DELETE
import Shared.Api.Handler.Document.Detail_Download_GET
import Shared.Api.Handler.Document.List_GET
import Shared.Api.Handler.Document.List_POST
import Shared.Api.Handler.WizardCommon

type DocumentAPI =
  Tags "Document"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_DELETE
           :<|> Detail_Download_GET
           :<|> Detail_Available_Submission_Services_GET
       )

documentApi :: Proxy DocumentAPI
documentApi = Proxy

documentServer :: WizardHandlerC s sm r rm => ServerT DocumentAPI sm
documentServer =
  list_GET :<|> list_POST :<|> detail_DELETE :<|> detail_download_GET :<|> detail_available_submission_Services_GET
