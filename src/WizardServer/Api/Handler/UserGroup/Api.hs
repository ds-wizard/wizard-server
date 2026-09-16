module WizardServer.Api.Handler.UserGroup.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.UserGroup.Detail_GET
import WizardServer.Api.Handler.UserGroup.List_Suggestions_GET

type UserGroupAPI = Tags "User Group" :> Detail_GET

type UserGroupSuggestionsAPI = Tags "User Group" :> List_Suggestions_GET

userGroupSuggestionsApi :: Proxy UserGroupSuggestionsAPI
userGroupSuggestionsApi = Proxy

userGroupSuggestionsServer :: WizardHandlerC s sm r rm => ServerT UserGroupSuggestionsAPI sm
userGroupSuggestionsServer = list_suggestions_GET

userGroupApi :: Proxy UserGroupAPI
userGroupApi = Proxy

userGroupServer :: WizardHandlerC s sm r rm => ServerT UserGroupAPI sm
userGroupServer = detail_GET
