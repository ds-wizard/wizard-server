module Shared.Api.Handler.DocumentTemplateDraft.File.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.DocumentTemplateDraft.File.Detail_Content_GET
import Shared.Api.Handler.DocumentTemplateDraft.File.Detail_Content_PUT
import Shared.Api.Handler.DocumentTemplateDraft.File.Detail_DELETE
import Shared.Api.Handler.DocumentTemplateDraft.File.Detail_GET
import Shared.Api.Handler.DocumentTemplateDraft.File.Detail_PUT
import Shared.Api.Handler.DocumentTemplateDraft.File.List_GET
import Shared.Api.Handler.DocumentTemplateDraft.File.List_POST
import Shared.Api.Handler.WizardCommon

type DocumentTemplateFileAPI =
  Tags "Document Template Draft File"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Detail_Content_GET
           :<|> Detail_Content_PUT
       )

documentTemplateFileApi :: Proxy DocumentTemplateFileAPI
documentTemplateFileApi = Proxy

documentTemplateFileServer :: WizardHandlerC s sm r rm => ServerT DocumentTemplateFileAPI sm
documentTemplateFileServer = list_GET :<|> list_POST :<|> detail_GET :<|> detail_PUT :<|> detail_DELETE :<|> detail_content_GET :<|> detail_content_PUT
