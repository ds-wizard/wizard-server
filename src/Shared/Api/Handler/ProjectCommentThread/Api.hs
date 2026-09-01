module Shared.Api.Handler.ProjectCommentThread.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.ProjectCommentThread.List_GET
import Shared.Api.Handler.WizardCommon

type ProjectCommentThreadAPI =
  Tags "Project Comment Thread"
    :> List_GET

projectCommentThreadApi :: Proxy ProjectCommentThreadAPI
projectCommentThreadApi = Proxy

projectCommentThreadServer :: WizardHandlerC s sm r rm => ServerT ProjectCommentThreadAPI sm
projectCommentThreadServer =
  list_GET
