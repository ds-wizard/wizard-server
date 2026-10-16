module Shared.Service.Websocket.WebsocketMapper where

import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.UUID as U

import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageDTO
import Shared.Api.Resource.Websocket.KnowledgeModelEditorMessageJM ()
import Shared.Api.Resource.Websocket.ProjectMessageDTO
import Shared.Api.Resource.Websocket.ProjectMessageJM ()
import Shared.Api.Resource.Websocket.WebsocketActionDTO
import Shared.Api.Resource.Websocket.WebsocketActionJM ()
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Error.Error
import Shared.Model.User.OnlineUserInfo
import Shared.Model.Websocket.WebsocketBusMessage
import Shared.Model.Websocket.WebsocketPerm
import Shared.Model.Websocket.WebsocketRegistry
import Shared.Model.Websocket.WebsocketSubscription

toUserListMessage :: WebsocketSubscription -> [OnlineUserInfo] -> Maybe Value
toUserListMessage ProjectWebsocketSubscription users =
  Just . toJSON . Success_ServerActionDTO . SetUserList_ServerProjectMessageDTO $ users
toUserListMessage KnowledgeModelEditorWebsocketSubscription users =
  Just . toJSON . Success_ServerActionDTO . SetUserList_ServerKnowledgeModelEditorMessageDTO $ users
toUserListMessage AnalyticsWebsocketSubscription _ = Nothing

toForceDisconnectError :: WebsocketSubscription -> U.UUID -> Maybe AppError
toForceDisconnectError ProjectWebsocketSubscription entityUuid =
  Just . NotExistsError $ _ERROR_SERVICE_PROJECT_COLLABORATION__FORCE_DISCONNECT (U.toString entityUuid)
toForceDisconnectError KnowledgeModelEditorWebsocketSubscription entityUuid =
  Just . NotExistsError $ _ERROR_SERVICE_KNOWLEDGE_MODEL_EDITOR__COLLABORATION__FORCE_DISCONNECT (U.toString entityUuid)
toForceDisconnectError AnalyticsWebsocketSubscription _ = Nothing

matchesRecipients :: WebsocketRecipients -> LocalWebsocketConnection -> Bool
matchesRecipients AllRecipients _ = True
matchesRecipients EditorsOnly connection = connection.websocketPerm == EditorWebsocketPerm
matchesRecipients CommentersAndEditors connection = connection.websocketPerm `elem` [EditorWebsocketPerm, CommenterWebsocketPerm]

toAnalyticsEtlFinishedMessage :: Value
toAnalyticsEtlFinishedMessage = object ["type" .= ("AnalyticsEtlFinished" :: String)]
