module WizardServer.Api.Handler.UserGroup.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.UserGroup.Detail_GET
import WizardServer.Api.Handler.UserGroup.List_Suggestions_GET

type UserGroupAPI =
  Tags "User Group"
    :> ( List_Suggestions_GET
           :<|> Detail_GET
       )

userGroupApi :: Proxy UserGroupAPI
userGroupApi = Proxy

userGroupServer :: WizardHandlerC s sm r rm => ServerT UserGroupAPI sm
userGroupServer =
  list_suggestions_GET
    :<|> detail_GET
