module Shared.Api.Resource.Websocket.WebsocketActionSM where

import Data.Swagger

import Shared.Api.Resource.Websocket.ProjectMessageSM ()
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Api.Resource.Websocket.WebsocketActionJM ()
import Shared.Database.Migration.Development.Project.Data.ProjectMessages
import Shared.Util.Swagger

instance ToSchema resDto => ToSchema (Success_ServerActionDTO resDto) where
  declareNamedSchema = toSwagger (Success_ServerActionDTO ensureOnlineUserAction)
