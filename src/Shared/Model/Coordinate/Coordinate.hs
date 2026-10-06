module Shared.Model.Coordinate.Coordinate where

import GHC.Generics

data Coordinate = Coordinate
  { id :: String
  , version :: String
  }
  deriving (Eq, Generic)

instance Show Coordinate where
  show (Coordinate {..}) = id ++ ":" ++ version

class CoordinateFactory entity where
  createCoordinate :: entity -> Coordinate
