module Shared.Service.Project.Collaboration.ProjectCollaborationMapper where

import Shared.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Shared.Api.Resource.Project.Event.ProjectEventDTO
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Model.Project.File.ProjectFileSimple

toSetReplyMessage :: SetReplyEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toSetReplyMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . SetReplyEventDTO'

toClearReplyMessage :: ClearReplyEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toClearReplyMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . ClearReplyEventDTO'

toSetPhaseMessage :: SetPhaseEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toSetPhaseMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . SetPhaseEventDTO'

toSetLabelMessage :: SetLabelsEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toSetLabelMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . SetLabelsEventDTO'

toResolveCommentThreadMessage :: ResolveCommentThreadEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toResolveCommentThreadMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . ResolveCommentThreadEventDTO'

toReopenCommentThreadMessage :: ReopenCommentThreadEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toReopenCommentThreadMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . ReopenCommentThreadEventDTO'

toAssignCommentThreadMessage :: AssignCommentThreadEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toAssignCommentThreadMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . AssignCommentThreadEventDTO'

toDeleteCommentThreadMessage :: DeleteCommentThreadEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toDeleteCommentThreadMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . DeleteCommentThreadEventDTO'

toAddCommentMessage :: AddCommentEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toAddCommentMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . AddCommentEventDTO'

toEditCommentMessage :: EditCommentEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toEditCommentMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . EditCommentEventDTO'

toDeleteCommentMessage :: DeleteCommentEventDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toDeleteCommentMessage = Success_ServerActionDTO . SetContent_ServerProjectMessageDTO . DeleteCommentEventDTO'

toSetProjectMessage :: ProjectDetailWsDTO -> Success_ServerActionDTO ServerProjectMessageDTO
toSetProjectMessage = Success_ServerActionDTO . SetProject_ServerProjectMessageDTO

toAddFileMessage :: ProjectFileSimple -> Success_ServerActionDTO ServerProjectMessageDTO
toAddFileMessage = Success_ServerActionDTO . AddFile_ServerProjectMessageDTO
