module Shared.Model.Locale.Locale where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Coordinate.Coordinate

data Locale = Locale
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
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord Locale where
  compare a b =
    compare a.id b.id
      <> compare a.version b.version

instance CoordinateFactory Locale where
  createCoordinate locale =
    Coordinate
      { id = locale.id
      , version = locale.version
      }
