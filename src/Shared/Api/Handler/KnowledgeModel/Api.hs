module Shared.Api.Handler.KnowledgeModel.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.KnowledgeModel.List_POST
import Shared.Api.Handler.WizardCommon

type KnowledgeModelAPI =
  Tags "Knowledge Model"
    :> List_POST

knowledgeModelApi :: Proxy KnowledgeModelAPI
knowledgeModelApi = Proxy

knowledgeModelServer :: WizardHandlerC s sm r rm => ServerT KnowledgeModelAPI sm
knowledgeModelServer = list_POST
