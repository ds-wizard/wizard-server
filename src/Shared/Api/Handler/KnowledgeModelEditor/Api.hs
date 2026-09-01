module Shared.Api.Handler.KnowledgeModelEditor.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.KnowledgeModelEditor.Detail_DELETE
import Shared.Api.Handler.KnowledgeModelEditor.Detail_GET
import Shared.Api.Handler.KnowledgeModelEditor.Detail_Locales_GET
import Shared.Api.Handler.KnowledgeModelEditor.Detail_PUT
import Shared.Api.Handler.KnowledgeModelEditor.Detail_WS
import Shared.Api.Handler.KnowledgeModelEditor.List_GET
import Shared.Api.Handler.KnowledgeModelEditor.List_POST
import Shared.Api.Handler.KnowledgeModelEditor.List_Suggestions_GET
import Shared.Api.Handler.KnowledgeModelEditor.Migration.Api
import Shared.Api.Handler.WizardCommon

type KnowledgeModelEditorAPI =
  Tags "Knowledge Model Editor"
    :> ( List_GET
           :<|> List_Suggestions_GET
           :<|> List_POST
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> Detail_Locales_GET
           :<|> Detail_WS
           :<|> MigrationAPI
       )

knowledgeModelEditorApi :: Proxy KnowledgeModelEditorAPI
knowledgeModelEditorApi = Proxy

knowledgeModelEditorServer :: WizardHandlerC s sm r rm => ServerT KnowledgeModelEditorAPI sm
knowledgeModelEditorServer =
  list_GET
    :<|> list_suggestions_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> detail_locales_GET
    :<|> detail_WS
    :<|> migrationServer
