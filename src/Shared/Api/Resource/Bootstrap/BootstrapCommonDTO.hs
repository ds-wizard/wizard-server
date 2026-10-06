module Shared.Api.Resource.Bootstrap.BootstrapCommonDTO where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Plugin.PluginList

data BootstrapPrivacyDTO = BootstrapPrivacyDTO
  { privacyUrl :: Maybe String
  , termsOfServiceUrl :: Maybe String
  }
  deriving (Generic, Eq, Show)

data BootstrapCloudDTO = BootstrapCloudDTO
  { enabled :: Bool
  , serverUrl :: String
  }
  deriving (Generic, Eq, Show)

data BootstrapSignalBridgeDTO = BootstrapSignalBridgeDTO
  { webSocketUrl :: Maybe String
  }
  deriving (Generic, Eq, Show)

data BootstrapModuleDTO = BootstrapModuleDTO
  { title :: String
  , description :: String
  , icon :: String
  , url :: String
  , external :: Bool
  }
  deriving (Generic, Eq, Show)

data BootstrapPlugins = BootstrapPlugins
  { plugins :: [PluginList]
  , pluginSettings :: M.Map U.UUID A.Value
  }
  deriving (Generic, Eq, Show)
