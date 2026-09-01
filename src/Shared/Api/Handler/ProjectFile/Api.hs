module Shared.Api.Handler.ProjectFile.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.ProjectFile.List_GET
import Shared.Api.Handler.WizardCommon

type ProjectFileAPI =
  Tags "Project File"
    :> List_GET

projectFileApi :: Proxy ProjectFileAPI
projectFileApi = Proxy

projectFileServer :: WizardHandlerC s sm r rm => ServerT ProjectFileAPI sm
projectFileServer =
  list_GET
