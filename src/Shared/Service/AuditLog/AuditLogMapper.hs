module Shared.Service.AuditLog.AuditLogMapper where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Time
import qualified Data.UUID as U
import GHC.Records

import Shared.Api.Resource.AuditLog.AuditLogDTO
import Shared.Api.Resource.AuditLog.AuditLogJM ()
import Shared.Api.Resource.User.UserDTO
import Shared.Model.AuditLog.AuditLog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.AuditLogSource
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.UserAuditLogData
import Shared.Util.Maybe

toAuditLog
  :: U.UUID
  -> String
  -> String
  -> Value
  -> Maybe (U.UUID, String)
  -> Maybe UserDTO
  -> AuditLogSource
  -> U.UUID
  -> UTCTime
  -> AuditLog
toAuditLog uuid component action actionData mWorkspace mCreatedBy source tenantUuid createdAt =
  AuditLog
    { uuid = uuid
    , component = component
    , action = action
    , actionData = actionData
    , workspaceUuid = fmap fst mWorkspace
    , workspaceName = fmap snd mWorkspace
    , createdByUuid = fmap (.uuid) mCreatedBy
    , createdByFirstName = fmap (.firstName) mCreatedBy
    , createdByLastName = fmap (.lastName) mCreatedBy
    , createdByEmail = fmap (.email) mCreatedBy
    , createdByImageUrl = concatMaybe . fmap (.imageUrl) $ mCreatedBy
    , sourceType = source.sType
    , sourceUuid = source.uuid
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    , sourceClientName = source.clientName
    , sourceToolName = source.toolName
    }

nonEmptyChanges :: ToJSON c => c -> Maybe Value
nonEmptyChanges changes =
  case toJSON changes of
    Object changesObject | not (KM.null changesObject) -> Just (Object changesObject)
    _ -> Nothing

toActionData :: ToJSON d => AuditLogEntity -> Maybe Value -> d -> Value
toActionData entity mChanges eventData =
  Object $ envelope <> dataObject (toJSON eventData)
  where
    envelope = KM.fromList (("entity", toJSON entity) : maybe [] (\changes -> [("changes", changes)]) mChanges)
    dataObject (Object o) = o
    dataObject _ = KM.empty

toDTO :: AuditLog -> AuditLogDTO
toDTO auditLog =
  AuditLogDTO
    { uuid = auditLog.uuid
    , component = auditLog.component
    , action = auditLog.action
    , actionData = auditLog.actionData
    , workspace = AuditLogWorkspaceDTO <$> auditLog.workspaceUuid <*> auditLog.workspaceName
    , createdBy = toUserDTO auditLog
    , source = toSource auditLog
    , createdAt = auditLog.createdAt
    }

toSource :: AuditLog -> AuditLogSource
toSource auditLog = AuditLogSource {sType = auditLog.sourceType, uuid = auditLog.sourceUuid, clientName = auditLog.sourceClientName, toolName = auditLog.sourceToolName}

toEventBridgeData :: AuditLog -> Value
toEventBridgeData auditLog =
  case toJSON (toDTO auditLog) of
    Object dto -> Object (KM.insert "tenantUuid" (toJSON auditLog.tenantUuid) dto)
    dto -> dto

toUserDTO :: AuditLog -> Maybe AuditLogUserDTO
toUserDTO auditLog =
  AuditLogUserDTO
    <$> auditLog.createdByUuid
    <*> auditLog.createdByFirstName
    <*> auditLog.createdByLastName
    <*> auditLog.createdByEmail
    <*> Just auditLog.createdByImageUrl

toUserRef
  :: (HasField "uuid" u U.UUID, HasField "firstName" u String, HasField "lastName" u String, HasField "email" u String, HasField "imageUrl" u (Maybe String))
  => u
  -> AuditLogUserRef
toUserRef user = AuditLogUserRef {uuid = user.uuid, firstName = user.firstName, lastName = user.lastName, email = user.email, imageUrl = user.imageUrl}

toSessionData
  :: (HasField "uuid" t U.UUID, HasField "name" t String, HasField "userAgent" t String, HasField "expiresAt" t UTCTime, HasField "createdAt" t UTCTime)
  => t
  -> AuditLogSessionData
toSessionData userToken =
  AuditLogSessionData
    { tokenUuid = userToken.uuid
    , name = userToken.name
    , userAgent = userToken.userAgent
    , expiresAt = userToken.expiresAt
    , createdAt = userToken.createdAt
    }

toAuditLogSource :: Maybe UserDTO -> AuditLogSource
toAuditLogSource (Just user) | user.uuid /= U.nil = userAuditLogSource
toAuditLogSource _ = systemAuditLogSource
