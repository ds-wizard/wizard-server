module Shared.Api.Handler.DocumentTemplateDraft.Folder.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.DocumentTemplateDraft.Folder.List_Delete_POST
import Shared.Api.Handler.DocumentTemplateDraft.Folder.List_Move_POST
import Shared.Api.Handler.WizardCommon

type DocumentTemplateFolderAPI =
  Tags "Document Template Draft Folder"
    :> ( List_Delete_POST
           :<|> List_Move_POST
       )

documentTemplateFolderApi :: Proxy DocumentTemplateFolderAPI
documentTemplateFolderApi = Proxy

documentTemplateFolderServer :: WizardHandlerC s sm r rm => ServerT DocumentTemplateFolderAPI sm
documentTemplateFolderServer =
  list_delete_POST
    :<|> list_move_POST
