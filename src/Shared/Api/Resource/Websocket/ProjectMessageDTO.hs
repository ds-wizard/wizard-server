module Shared.Api.Resource.Websocket.ProjectMessageDTO where

import GHC.Generics

import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Model.User.OnlineUserInfo

data ClientProjectMessageDTO = SetContent_ClientProjectMessageDTO
  { aData :: ProjectEventChangeDTO
  }
  deriving (Show, Generic)

data ServerProjectMessageDTO
  = SetUserList_ServerProjectMessageDTO
      { ouiData :: [OnlineUserInfo]
      }
  | SetContent_ServerProjectMessageDTO
      { qeData :: ProjectEventDTO
      }
  | SetProject_ServerProjectMessageDTO
      { sqData :: ProjectDetailWsDTO
      }
  | AddFile_ServerProjectMessageDTO
      { adData :: ProjectFileSimple
      }
  deriving (Show, Eq, Generic)
