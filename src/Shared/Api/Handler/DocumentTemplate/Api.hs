module Shared.Api.Handler.DocumentTemplate.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.DocumentTemplate.Detail_Bundle_GET
import Shared.Api.Handler.DocumentTemplate.Detail_DELETE
import Shared.Api.Handler.DocumentTemplate.Detail_GET
import Shared.Api.Handler.DocumentTemplate.Detail_Locales_Content_GET
import Shared.Api.Handler.DocumentTemplate.Detail_Locales_DELETE
import Shared.Api.Handler.DocumentTemplate.Detail_Locales_GET
import Shared.Api.Handler.DocumentTemplate.Detail_Locales_POST
import Shared.Api.Handler.DocumentTemplate.Detail_Locales_Template_GET
import Shared.Api.Handler.DocumentTemplate.Detail_PUT
import Shared.Api.Handler.DocumentTemplate.Detail_Pull_POST
import Shared.Api.Handler.DocumentTemplate.List_All_GET
import Shared.Api.Handler.DocumentTemplate.List_Bundle_POST
import Shared.Api.Handler.DocumentTemplate.List_DELETE
import Shared.Api.Handler.DocumentTemplate.List_GET
import Shared.Api.Handler.DocumentTemplate.List_Suggestions_GET
import Shared.Api.Handler.WizardCommon

type DocumentTemplateAPI =
  Tags "Document Template"
    :> ( List_GET
           :<|> List_All_GET
           :<|> List_Suggestions_GET
           :<|> List_DELETE
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> List_Bundle_POST
           :<|> Detail_Bundle_GET
           :<|> Detail_Pull_POST
           :<|> Detail_Locales_GET
           :<|> Detail_Locales_POST
           :<|> Detail_Locales_Template_GET
           :<|> Detail_Locales_Content_GET
           :<|> Detail_Locales_DELETE
       )

documentTemplateApi :: Proxy DocumentTemplateAPI
documentTemplateApi = Proxy

documentTemplateServer :: WizardHandlerC s sm r rm => ServerT DocumentTemplateAPI sm
documentTemplateServer =
  list_GET
    :<|> list_all_GET
    :<|> list_suggestions_GET
    :<|> list_DELETE
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> list_bundle_POST
    :<|> detail_bundle_GET
    :<|> detail_pull_POST
    :<|> detail_locales_GET
    :<|> detail_locales_POST
    :<|> detail_locales_template_GET
    :<|> detail_locales_content_GET
    :<|> detail_locales_DELETE
