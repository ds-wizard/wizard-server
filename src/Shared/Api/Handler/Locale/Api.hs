module Shared.Api.Handler.Locale.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Locale.Detail_DELETE
import Shared.Api.Handler.Locale.Detail_PUT
import Shared.Api.Handler.Locale.Detail_Po_GET
import Shared.Api.Handler.Locale.List_Current_Content_GET
import Shared.Api.Handler.Locale.List_GET
import Shared.Api.Handler.Locale.List_POST
import Shared.Api.Handler.Locale.List_Suggestions_GET
import Shared.Api.Handler.WizardCommon

type LocaleAPI =
  Tags "Locale"
    :> ( List_GET
           :<|> List_Suggestions_GET
           :<|> List_Current_Content_GET
           :<|> List_POST
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Detail_Po_GET
       )

localeApi :: Proxy LocaleAPI
localeApi = Proxy

localeServer :: WizardHandlerC s sm r rm => ServerT LocaleAPI sm
localeServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_current_content_GET
    :<|> list_POST
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_po_GET
