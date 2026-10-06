module Shared.Api.Handler.UserGroup.Membership.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.UserGroup.Membership.List_PUT
import Shared.Api.Handler.WizardCommon

type UserGroupMembershipAPI =
  Tags "User Group Membership"
    :> List_PUT

userGroupMembershipApi :: Proxy UserGroupMembershipAPI
userGroupMembershipApi = Proxy

userGroupMembershipServer :: WizardHandlerC s sm r rm => ServerT UserGroupMembershipAPI sm
userGroupMembershipServer = list_PUT
