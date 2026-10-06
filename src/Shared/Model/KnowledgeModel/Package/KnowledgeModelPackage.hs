module Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Coordinate.Coordinate

data KnowledgeModelPackagePhase
  = ReleasedKnowledgeModelPackagePhase
  | DeprecatedKnowledgeModelPackagePhase
  deriving (Show, Eq, Generic, Read)

data KnowledgeModelPackage = KnowledgeModelPackage
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , metamodelVersion :: Int
  , description :: String
  , readme :: String
  , license :: String
  , language :: String
  , previousPackageUuid :: Maybe U.UUID
  , forkOfPackageId :: Maybe Coordinate
  , mergeCheckpointPackageId :: Maybe Coordinate
  , nonEditable :: Bool
  , public :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance Ord KnowledgeModelPackage where
  compare a b =
    compare a.id b.id
      <> compare a.version b.version

instance CoordinateFactory KnowledgeModelPackage where
  createCoordinate p =
    Coordinate
      { id = p.id
      , version = p.version
      }
