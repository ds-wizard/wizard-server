module Shared.Model.Context.WizardServerContext where

import Control.Concurrent.MVar (MVar)
import GHC.Records
import Network.HTTP.Client (Manager)
import Servant.Client (ClientEnv)

import Shared.Model.Cache.ServerCache
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.ServerContext

class
  ( ServerContextType context ServerConfig
  , HasField "cache'" context ServerCache
  , HasField "registryClient'" context ClientEnv
  , HasField "restrictedHttpClientManager'" context Manager
  , HasField "shutdownFlag'" context (MVar ())
  ) =>
  WizardServerContextType context

class (ServerContextC context ServerConfig m, WizardServerContextType context) => WizardServerContextC context m | m -> context
