module Shared.Api.Handler.Project.Migration.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.Migration.List_POST
import Shared.Api.Handler.WizardCommon

type MigrationAPI =
  Tags "Project Migration"
    :> List_POST

migrationApi :: Proxy MigrationAPI
migrationApi = Proxy

migrationServer :: WizardHandlerC s sm r rm => ServerT MigrationAPI sm
migrationServer = list_POST
