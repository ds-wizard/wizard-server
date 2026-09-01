module Shared.Api.Handler.KnowledgeModelEditor.Migration.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_Conflict_All_POST
import Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_Conflict_POST
import Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_DELETE
import Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_GET
import Shared.Api.Handler.KnowledgeModelEditor.Migration.List_Current_POST
import Shared.Api.Handler.WizardCommon

type MigrationAPI =
  Tags "Knowledge Model Editor Migration"
    :> ( List_Current_GET
           :<|> List_Current_POST
           :<|> List_Current_DELETE
           :<|> List_Current_Conflict_POST
           :<|> List_Current_Conflict_All_POST
       )

migrationApi :: Proxy MigrationAPI
migrationApi = Proxy

migrationServer :: WizardHandlerC s sm r rm => ServerT MigrationAPI sm
migrationServer =
  list_current_GET
    :<|> list_current_POST
    :<|> list_current_DELETE
    :<|> list_current_conflict_POST
    :<|> list_Current_Conflict_All_POST
