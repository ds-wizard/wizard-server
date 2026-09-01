module Shared.Api.Handler.Project.User.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Project.User.List_Suggestions_GET
import Shared.Api.Handler.WizardCommon

type UserAPI =
  Tags "Project User"
    :> List_Suggestions_GET

userApi :: Proxy UserAPI
userApi = Proxy

userServer :: WizardHandlerC s sm r rm => ServerT UserAPI sm
userServer = list_suggestions_GET
