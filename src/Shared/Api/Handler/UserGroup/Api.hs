module Shared.Api.Handler.UserGroup.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.UserGroup.Detail_DELETE
import Shared.Api.Handler.UserGroup.Detail_GET
import Shared.Api.Handler.UserGroup.Detail_PUT
import Shared.Api.Handler.UserGroup.List_GET
import Shared.Api.Handler.UserGroup.List_POST
import Shared.Api.Handler.UserGroup.List_Suggestions_GET
import Shared.Api.Handler.UserGroup.Membership.Api
import Shared.Api.Handler.WizardCommon

type UserGroupAPI =
  Tags "User Group"
    :> ( List_GET
           :<|> List_POST
           :<|> List_Suggestions_GET
           :<|> Detail_GET
           :<|> Detail_PUT
           :<|> Detail_DELETE
           :<|> UserGroupMembershipAPI
       )

userGroupApi :: Proxy UserGroupAPI
userGroupApi = Proxy

userGroupServer :: WizardHandlerC s sm r rm => ServerT UserGroupAPI sm
userGroupServer =
  list_GET
    :<|> list_POST
    :<|> list_suggestions_GET
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> userGroupMembershipServer
