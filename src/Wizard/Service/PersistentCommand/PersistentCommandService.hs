module Wizard.Service.PersistentCommand.PersistentCommandService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.PersistentCommand.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommandSimple
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandService
import Wizard.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextMappers
import Wizard.Service.PersistentCommand.PersistentCommandExecutor
import Wizard.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Service.Tenant.TenantUtil
import qualified Wizard.Service.User.UserMapper as UM
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

getPersistentCommandsPage :: [String] -> Pageable -> [Sort] -> AppContextM (Page PersistentCommandList)
getPersistentCommandsPage states pageable sort = do
  checkPermission _DEV_USE_ROLE_PERMISSION
  findPersistentCommandsPage states pageable sort

getPersistentCommandById :: U.UUID -> AppContextM PersistentCommandDetailDTO
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

modifyPersistentCommand :: U.UUID -> PersistentCommandChangeDTO -> AppContextM PersistentCommandDetailDTO
modifyPersistentCommand uuid reqDto = do
  checkPermission _DEV_USE_ROLE_PERMISSION
  command <- findPersistentCommandByUuid uuid
  now <- liftIO getCurrentTime
  let updatedCommand = fromChangeDTO command reqDto now :: PersistentCommand U.UUID
  updatePersistentCommandByUuid updatedCommand
  getPersistentCommandById uuid

runPersistentCommandById :: U.UUID -> AppContextM PersistentCommandDetailDTO
runPersistentCommandById uuid = do
  command <- findPersistentCommandByUuid uuid
  if command.internal
    then runPersistentCommand' True (toSimple command)
    else do
      notifySpecificPersistentCommandQueue command
      return ()
  getPersistentCommandById uuid

runPersistentCommands' :: AppContextM ()
runPersistentCommands' = runPersistentCommands runAppContextWithAppContext' updateContext execute

runPersistentCommand' :: Bool -> PersistentCommandSimple U.UUID -> AppContextM ()
runPersistentCommand' = runPersistentCommand runAppContextWithAppContext' updateContext execute

runPersistentCommandChannelListener' :: AppContextM ()
runPersistentCommandChannelListener' = runPersistentCommandChannelListener runAppContextWithAppContext' updateContext execute

updateContext :: PersistentCommandSimple U.UUID -> AppContext -> AppContextM AppContext
updateContext commandSimple context = do
  user <-
    case commandSimple.createdBy of
      Just userUuid -> findUserByUuidSystem' userUuid commandSimple.tenantUuid
      Nothing -> return Nothing
  return $
    context
      { currentTenantUuid = commandSimple.tenantUuid
      , currentUser = fmap UM.toDTO user
      }
