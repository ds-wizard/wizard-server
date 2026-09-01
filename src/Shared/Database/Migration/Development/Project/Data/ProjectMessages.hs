module Shared.Database.Migration.Development.Project.Data.ProjectMessages where

import Shared.Api.Resource.Project.Event.ProjectEventChangeDTO
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Service.Project.Event.ProjectEventMapper

ensureOnlineUserAction :: ClientProjectMessageDTO
ensureOnlineUserAction =
  SetContent_ClientProjectMessageDTO . SetReplyEventChangeDTO' $
    toSetReplyEventChangeDTO (sre_rQ1 project1Uuid)

setUserListAction :: ServerProjectMessageDTO
setUserListAction = SetUserList_ServerProjectMessageDTO [userAlbertOnlineInfo]
