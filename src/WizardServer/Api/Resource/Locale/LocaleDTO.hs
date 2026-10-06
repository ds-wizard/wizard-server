module WizardServer.Api.Resource.Locale.LocaleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data LocaleDTO = LocaleDTO
  { uuid :: U.UUID
  , name :: String
  , description :: String
  , code :: String
  , id :: String
  , version :: String
  , defaultLocale :: Bool
  , enabled :: Bool
  , remoteLatestVersion :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
