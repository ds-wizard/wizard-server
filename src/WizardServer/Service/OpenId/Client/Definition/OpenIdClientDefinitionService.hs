module WizardServer.Service.OpenId.Client.Definition.OpenIdClientDefinitionService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import Shared.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.OpenId.OpenIdClientSimple
import Shared.Service.OpenId.Client.Definition.OpenIdClientDefinitionMapper
import Shared.Util.Uuid

getOpenIdClientDefinitions :: WizardRequestContextC s m => m [OpenIdClientSimple]
getOpenIdClientDefinitions = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  openIdClients <- findOpenIdClientDefinitions
  return . fmap toSimple $ openIdClients

getOpenIdClientDefinitionByUuid :: WizardRequestContextC s m => U.UUID -> m OpenIdClientDetailDTO
getOpenIdClientDefinitionByUuid uuid = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  openIdClient <- findOpenIdClientDefinitionByUuid uuid
  return $ toDetailDTO openIdClient

createOpenIdClientDefinition :: WizardRequestContextC s m => OpenIdClientChangeDTO -> m OpenIdClientDetailDTO
createOpenIdClientDefinition reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    uuid <- liftIO generateUuid
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    let openIdClient = fromCreateDTO reqDto uuid tenantUuid now
    void $ insertOpenIdClientDefinition openIdClient
    return $ toDetailDTO openIdClient

modifyOpenIdClientDefinition :: WizardRequestContextC s m => U.UUID -> OpenIdClientChangeDTO -> m OpenIdClientDetailDTO
modifyOpenIdClientDefinition uuid reqDto =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    openIdClient <- findOpenIdClientDefinitionByUuid uuid
    now <- liftIO getCurrentTime
    let updatedOpenIdClient = fromChangeDTO openIdClient reqDto now
    void $ updateOpenIdClientDefinition updatedOpenIdClient
    return $ toDetailDTO updatedOpenIdClient

deleteOpenIdClientDefinition :: WizardRequestContextC s m => U.UUID -> m ()
deleteOpenIdClientDefinition uuid =
  runInTransaction $ do
    checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
    _ <- findOpenIdClientDefinitionByUuid uuid
    deleteOpenIdClientDefinitionByUuid uuid
