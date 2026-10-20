module Shared.Service.Mcp.Tool.McpExecutableServices where

import qualified Data.UUID as U

import Shared.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandChangeDTO
import Shared.Api.Resource.UserToken.ApiKeyCreateDTO
import Shared.Api.Resource.UserToken.UserTokenDTO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Dev.Dev
import Shared.Model.PersistentCommand.PersistentCommandList

data McpExecutableServices m = McpExecutableServices
  { findUserLoginProviders :: U.UUID -> m [U.UUID]
  , setUserPassword :: U.UUID -> String -> m ()
  , sendPasswordLink :: String -> m ()
  , createApiKey :: ApiKeyCreateDTO -> Maybe String -> m UserTokenDTO
  , deleteToken :: U.UUID -> m ()
  , deleteUser :: U.UUID -> m ()
  , createOpenIdClient :: U.UUID -> OpenIdClientChangeDTO -> m ()
  , modifyOpenIdClient :: U.UUID -> OpenIdClientChangeDTO -> m ()
  , deleteOpenIdClient :: U.UUID -> m ()
  }

data McpOperationServices m command = McpOperationServices
  { devSections :: m [DevSection m]
  , getPersistentCommandsPage :: [String] -> Pageable -> [Sort] -> m (Page PersistentCommandList)
  , getPersistentCommandById :: U.UUID -> m command
  , modifyPersistentCommand :: U.UUID -> PersistentCommandChangeDTO -> m command
  , runPersistentCommandById :: U.UUID -> m command
  }
