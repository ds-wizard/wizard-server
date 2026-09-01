module Shared.Api.Handler.Project.Tag.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.Tag.List_Suggestions_GET
import Shared.Api.Handler.WizardCommon

type TagAPI =
  Tags "Project Tag"
    :> List_Suggestions_GET

tagApi :: Proxy TagAPI
tagApi = Proxy

tagServer :: WizardHandlerC s sm r rm => ServerT TagAPI sm
tagServer = list_suggestions_GET
