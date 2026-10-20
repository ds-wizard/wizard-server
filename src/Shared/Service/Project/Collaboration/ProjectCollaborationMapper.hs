module Shared.Service.Project.Collaboration.ProjectCollaborationMapper where

import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Model.Project.File.ProjectFileSimple

toAddEventMessage :: ProjectEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toAddEventMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO

toSetProjectMessage :: ProjectDetailWsDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toSetProjectMessage = Success_ServerActionDTO . SetProject_ServerProjectMessageDTO

toAddFileMessage :: ProjectFileSimple -> Success_ServerActionDTO ServerProjectMessageDTO
toAddFileMessage = Success_ServerActionDTO . AddFile_ServerProjectMessageDTO
