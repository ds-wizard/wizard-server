module Shared.Api.Handler.KnowledgeModelSecret.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.KnowledgeModelSecret.Detail_DELETE
import Shared.Api.Handler.KnowledgeModelSecret.Detail_PUT
import Shared.Api.Handler.KnowledgeModelSecret.List_GET
import Shared.Api.Handler.KnowledgeModelSecret.List_POST
import Shared.Api.Handler.WizardCommon

type KnowledgeModelSecretAPI =
  Tags "KnowledgeModelSecret"
    :> ( List_GET
           :<|> List_POST
           :<|> Detail_PUT
           :<|> Detail_DELETE
       )

knowledgeModelSecretApi :: Proxy KnowledgeModelSecretAPI
knowledgeModelSecretApi = Proxy

knowledgeModelSecretServer :: WizardHandlerC s sm r rm => ServerT KnowledgeModelSecretAPI sm
knowledgeModelSecretServer =
  list_GET
    :<|> list_POST
    :<|> detail_PUT
    :<|> detail_DELETE
