module Shared.Api.Handler.Project.Comment.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.Comment.List_GET
import Shared.Api.Handler.WizardCommon

type CommentAPI =
  Tags "Project Comment"
    :> List_GET

commentApi :: Proxy CommentAPI
commentApi = Proxy

commentServer :: WizardHandlerC s sm r rm => ServerT CommentAPI sm
commentServer = list_GET
