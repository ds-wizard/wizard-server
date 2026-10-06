module Shared.Api.Resource.LocaleBundle.LocaleBundleDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Coordinate.Coordinate

data LocaleBundleDTO = LocaleBundleDTO
  { id :: String
  , name :: String
  , description :: String
  , code :: String
  , version :: String
  , license :: String
  , readme :: String
  , recommendedAppVersion :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory LocaleBundleDTO where
  createCoordinate locale =
    Coordinate
      { id = locale.id
      , version = locale.version
      }
