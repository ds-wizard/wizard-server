module Shared.Api.Resource.Websocket.ProjectMessageSM where

import Data.Swagger

import Shared.Api.Resource.Project.Detail.ProjectDetailWsSM ()
import Shared.Api.Resource.Project.Event.ProjectEventChangeSM ()
import Shared.Api.Resource.Project.Event.ProjectEventSM ()
import Shared.Api.Resource.Project.File.ProjectFileSimpleSM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Api.Resource.User.OnlineUserInfoSM ()
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.ProjectMessageJM ()
import Shared.Database.Migration.Development.Project.Data.ProjectMessages
import Shared.Util.Swagger

instance ToSchema ClientProjectMessageDTO where
  declareNamedSchema = toSwagger ensureOnlineUserAction

instance ToSchema ServerProjectMessageDTO where
  declareNamedSchema = toSwagger setUserListAction
