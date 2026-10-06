module Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage where

import Data.Time
import GHC.Generics

import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelBundlePackage = KnowledgeModelBundlePackage
  { id :: String
  , name :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , language :: String
  , previousPackageId :: Maybe Coordinate
  , forkOfPackageId :: Maybe Coordinate
  , mergeCheckpointPackageId :: Maybe Coordinate
  , events :: [KnowledgeModelEvent]
  , nonEditable :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory KnowledgeModelBundlePackage where
  createCoordinate dt =
    Coordinate
      { id = dt.id
      , version = dt.version
      }
