module WizardServer.Api.Handler.UserEmailLink.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.UserEmailLink.List_POST

type UserEmailLinkAPI =
  Tags "User Email Link"
    :> List_POST

userEmailLinkApi :: Proxy UserEmailLinkAPI
userEmailLinkApi = Proxy

userEmailLinkServer :: WizardHandlerC s sm r rm => ServerT UserEmailLinkAPI sm
userEmailLinkServer = list_POST
