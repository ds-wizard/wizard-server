module Shared.Model.Context.WizardRequestContext where

import Control.Concurrent.MVar (MVar)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection)
import GHC.Records
import Network.HTTP.Client (Manager)
import Servant.Client (ClientEnv)

import Shared.Api.Resource.User.UserDTO
import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.RequestContext

class
  ( RequestContextType context ServerConfig
  , HasField "currentUser'" context (Maybe UserDTO)
  , HasField "identityEmail'" context (Maybe String)
  , HasField "cache'" context ServerCache
  , HasField "registryClient'" context ClientEnv
  , HasField "restrictedHttpClientManager'" context Manager
  , HasField "shutdownFlag'" context (MVar ())
  ) =>
  WizardRequestContextType context
  where
  setTenantUuid :: U.UUID -> context -> context
  setCurrentUser :: Maybe UserDTO -> context -> context
  setTraceUuid :: U.UUID -> context -> context
  setDbConnection :: Maybe Connection -> context -> context

class (RequestContextC context ServerConfig m, WizardRequestContextType context) => WizardRequestContextC context m | m -> context where
  runRequestContextWithRequestContext :: m a -> context -> IO (Either String a)
