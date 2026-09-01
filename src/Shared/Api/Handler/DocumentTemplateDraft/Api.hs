module Shared.Api.Handler.DocumentTemplateDraft.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.DocumentTemplateDraft.Detail_DELETE
import Shared.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_GET
import Shared.Api.Handler.DocumentTemplateDraft.Detail_Documents_Preview_Settings_PUT
import Shared.Api.Handler.DocumentTemplateDraft.Detail_GET
import Shared.Api.Handler.DocumentTemplateDraft.Detail_PUT
import Shared.Api.Handler.DocumentTemplateDraft.List_GET
import Shared.Api.Handler.DocumentTemplateDraft.List_POST
import Shared.Api.Handler.WizardCommon

type DocumentTemplateDraftAPI =
  Tags "Document Template Draft"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Detail_Documents_Preview_GET
           :<|> Detail_Documents_Preview_Settings_PUT
       )

documentTemplateDraftApi :: Proxy DocumentTemplateDraftAPI
documentTemplateDraftApi = Proxy

documentTemplateDraftServer :: WizardHandlerC s sm r rm => ServerT DocumentTemplateDraftAPI sm
documentTemplateDraftServer =
  list_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_documents_preview_GET
    :<|> detail_documents_preview_settings_PUT
