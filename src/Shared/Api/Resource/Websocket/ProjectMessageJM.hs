module Shared.Api.Resource.Websocket.ProjectMessageJM where

import Data.Aeson

import Shared.Api.Resource.Project.Detail.ProjectDetailWsJM ()
import Shared.Api.Resource.Project.Event.ProjectEventChangeJM ()
import Shared.Api.Resource.Project.Event.ProjectEventJM ()
import Shared.Api.Resource.Project.File.ProjectFileSimpleJM ()
import Shared.Api.Resource.Project.ProjectReplyJM ()
import Shared.Api.Resource.User.OnlineUserInfoJM ()
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Util.Aeson

instance FromJSON ClientProjectMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientProjectMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ServerProjectMessageDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ServerProjectMessageDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
