module Shared.Api.Handler.KnowledgeModelEditor.Event.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.KnowledgeModelEditor.Event.List_POST
import Shared.Api.Handler.WizardCommon

type EventAPI =
  Tags "Knowledge Model Editor Event"
    :> List_POST

eventApi :: Proxy EventAPI
eventApi = Proxy

eventServer :: WizardHandlerC s sm r rm => ServerT EventAPI sm
eventServer = list_POST
