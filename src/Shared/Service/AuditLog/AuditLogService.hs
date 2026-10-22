module Shared.Service.AuditLog.AuditLogService (
  logAuditLog,
  logAuditLogNoData,
  logAuditLogUpdate,
  logAuditLogChanges,
  logAuditLogWithChanges,
  whenAuditLogEnabled,
  asUser,
) where

import Control.Monad (forM_, mfilter, when)
import Control.Monad.Reader (ask, asks, liftIO, local)
import Data.Aeson
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.User.UserDTO (UserDTO (..))
import Shared.Constant.PersistentCommand
import Shared.Database.DAO.AuditLog.AuditLogDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.Workspace.WorkspaceDAO
import Shared.Model.AuditLog.AuditLog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.AuditLogEventDefinition
import Shared.Model.AuditLog.AuditLogScope
import Shared.Model.AuditLog.AuditLogSource
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.PersistentCommand.Aws.AwsEventBridgePutEvents
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Service.AuditLog.AuditLogMapper
import Shared.Service.PersistentCommand.PersistentCommandMapper
import qualified Shared.Service.User.WizardUserMapper as UM
import Shared.Util.JSON
import Shared.Util.String
import Shared.Util.Uuid

logAuditLog :: (WizardRequestContextC s m, ToJSON d) => AuditLogEventDefinition () d -> AuditLogEntity -> d -> m ()
logAuditLog definition entity = writeAuditLog definition entity Nothing

logAuditLogNoData :: WizardRequestContextC s m => AuditLogEventDefinition () NoData -> AuditLogEntity -> m ()
logAuditLogNoData definition entity = logAuditLog definition entity NoData

logAuditLogUpdate :: (WizardRequestContextC s m, ToJSON c, ToJSON d) => AuditLogEventDefinition c d -> AuditLogEntity -> c -> d -> m ()
logAuditLogUpdate definition entity changes eventData =
  forM_ (nonEmptyChanges changes) (\changesValue -> writeAuditLog definition entity (Just changesValue) eventData)

logAuditLogChanges :: (WizardRequestContextC s m, ToJSON c) => AuditLogEventDefinition c NoData -> AuditLogEntity -> c -> m ()
logAuditLogChanges definition entity changes = logAuditLogUpdate definition entity changes NoData

logAuditLogWithChanges :: (WizardRequestContextC s m, ToJSON c, ToJSON d) => AuditLogEventDefinition c d -> AuditLogEntity -> c -> d -> m ()
logAuditLogWithChanges definition entity changes = writeAuditLog definition entity (nonEmptyChanges changes)

whenAuditLogEnabled :: WizardRequestContextC s m => m () -> m ()
whenAuditLogEnabled action = do
  enabled <- asks (.auditLogEnabled')
  when enabled action

asUser :: WizardRequestContextC s m => User -> m a -> m a
asUser user action = do
  mCurrentUser <- asks (mfilter (\currentUser -> currentUser.uuid /= U.nil) . (.currentUser'))
  case mCurrentUser of
    Nothing -> local (setAuditLogSource userAuditLogSource . setCurrentUser (Just (UM.toDTO user))) action
    Just _ -> action

writeAuditLog :: (WizardRequestContextC s m, ToJSON d) => AuditLogEventDefinition c d -> AuditLogEntity -> Maybe Value -> d -> m ()
writeAuditLog definition entity mChanges eventData =
  whenAuditLogEnabled $ do
    mWorkspace <- findWorkspaceSnapshot (workspaceOf definition entity)
    let snapshot = AuditLogEntity {uuid = entity.uuid, name = entity.name, workspaceUuid = fmap fst mWorkspace}
    let actionData = toActionData snapshot mChanges eventData
    auditLog <- createAuditLog definition actionData mWorkspace
    insertAuditLog auditLog
    publishToEventBridge definition auditLog

workspaceOf :: AuditLogEventDefinition c d -> AuditLogEntity -> Maybe U.UUID
workspaceOf definition entity =
  case definitionScope definition of
    OrganizationAuditLogScope -> Nothing
    _ -> entity.workspaceUuid

findWorkspaceSnapshot :: WizardRequestContextC s m => Maybe U.UUID -> m (Maybe (U.UUID, String))
findWorkspaceSnapshot = traverse (\workspaceUuid -> (\mWorkspace -> (workspaceUuid, maybe "" (.name) mWorkspace)) <$> findWorkspaceByUuid' workspaceUuid)

createAuditLog :: WizardRequestContextC s m => AuditLogEventDefinition c d -> Value -> Maybe (U.UUID, String) -> m AuditLog
createAuditLog definition actionData mWorkspace = do
  uuid <- liftIO generateUuid
  tenantUuid <- asks (.tenantUuid')
  mCreatedBy <- asks (mfilter (\user -> user.uuid /= U.nil) . (.currentUser'))
  source <- asks (.auditLogSource')
  now <- liftIO getCurrentTime
  return $ toAuditLog uuid (definitionComponent definition) (definitionAction definition) actionData mWorkspace mCreatedBy source tenantUuid now

publishToEventBridge :: WizardRequestContextC s m => AuditLogEventDefinition c d -> AuditLog -> m ()
publishToEventBridge definition auditLog = do
  context <- ask
  when (context.serverConfig'.auditLog.eventBridgePublishingEnabled && definitionEventBridge definition) $ do
    uuid <- liftIO generateUuid
    let action = f' "%s::%s" [auditLog.component, auditLog.action]
    let body = encodeJsonToString AwsEventBridgePutEvents {action = action, actionData = toEventBridgeData auditLog}
    let createdBy = fmap U.toString auditLog.createdByUuid
    insertPersistentCommand (toPersistentCommand uuid awsEventBridgeComponent "putEvents" body 10 auditLog.tenantUuid createdBy auditLog.createdAt)
    return ()
