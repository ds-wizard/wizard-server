module Shared.Service.PersistentCommand.WizardPersistentCommandService where

import Control.Monad (void)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.PersistentCommand.WizardPersistentCommandDAO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.ContextMappers
import Shared.Model.Context.WizardRequestContext
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Model.PersistentCommand.PersistentCommandSimple
import Shared.Service.PersistentCommand.PersistentCommandExecutor
import Shared.Service.PersistentCommand.PersistentCommandMapper
import Shared.Service.PersistentCommand.PersistentCommandService
import Shared.Service.PersistentCommand.WizardPersistentCommandMapper
import Shared.Service.Tenant.TenantUtil
import qualified Shared.Service.User.WizardUserMapper as UM

getPersistentCommandsPage :: WizardRequestContextC s m => [String] -> Pageable -> [Sort] -> m (Page PersistentCommandList)
getPersistentCommandsPage states pageable sort = do
  checkPermission _DEV_USE_ROLE_PERMISSION
  findPersistentCommandsPage states pageable sort

getPersistentCommandById :: WizardRequestContextC s m => U.UUID -> m PersistentCommandDetailDTO
getPersistentCommandById uuid = do
  checkPermission _DEV_USE_ROLE_PERMISSION
  command <- findPersistentCommandByUuid uuid
  mUser <-
    case command.createdBy of
      Just userUuid -> findUserByUuidSystem' userUuid command.tenantUuid
      Nothing -> return Nothing
  tenant <- findTenantByUuid command.tenantUuid
  tenantDto <- enhanceTenant tenant
  return $ toDetailDTO command mUser tenantDto

modifyPersistentCommand :: WizardRequestContextC s m => U.UUID -> PersistentCommandChangeDTO -> m PersistentCommandDetailDTO
modifyPersistentCommand uuid reqDto = do
  checkPermission _DEV_USE_ROLE_PERMISSION
  command <- findPersistentCommandByUuid uuid
  now <- liftIO getCurrentTime
  let updatedCommand = fromChangeDTO command reqDto now :: PersistentCommand U.UUID
  updatePersistentCommandByUuid updatedCommand
  getPersistentCommandById uuid

runPersistentCommandById :: WizardRequestContextC s m => U.UUID -> m PersistentCommandDetailDTO
runPersistentCommandById uuid = do
  command <- findPersistentCommandByUuid uuid
  if command.component `elem` components
    then runPersistentCommand' True (toSimple command)
    else void $ notifyPersistentCommandQueues command
  getPersistentCommandById uuid

runPersistentCommands' :: forall s m. WizardRequestContextC s m => m ()
runPersistentCommands' = runPersistentCommands (runWithConnection @s @m) updateContext execute components

runPersistentCommand' :: forall s m. WizardRequestContextC s m => Bool -> PersistentCommandSimple U.UUID -> m ()
runPersistentCommand' = runPersistentCommand (runWithConnection @s @m) updateContext execute

runPersistentCommandChannelListener' :: forall s m. WizardRequestContextC s m => m ()
runPersistentCommandChannelListener' = runPersistentCommandChannelListener (runWithConnection @s @m) updateContext execute components

updateContext :: WizardRequestContextC s m => PersistentCommandSimple U.UUID -> s -> m s
updateContext commandSimple context = do
  user <-
    case commandSimple.createdBy of
      Just userUuid -> findUserByUuidSystem' userUuid commandSimple.tenantUuid
      Nothing -> return Nothing
  return . setCurrentUser (fmap UM.toDTO user) . setTenantUuid commandSimple.tenantUuid $ context
