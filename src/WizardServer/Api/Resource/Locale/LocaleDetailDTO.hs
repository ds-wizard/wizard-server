module WizardServer.Api.Resource.Locale.LocaleDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Version.VersionDTO

data LocaleDetailDTO = LocaleDetailDTO
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , code :: String
  , id :: String
  , version :: String
  , defaultLocale :: Bool
  , license :: String
  , readme :: String
  , recommendedAppVersion :: String
  , enabled :: Bool
  , versions :: [VersionDTO]
  , remoteLatestVersion :: Maybe String
  , registryLink :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
