module RegistryPublic.Api.Resource.Locale.LocaleDTO where

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
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
