module Shared.Api.Resource.Bootstrap.BootstrapCommonDTO where

import GHC.Generics

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
