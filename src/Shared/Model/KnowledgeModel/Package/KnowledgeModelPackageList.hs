module Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage (KnowledgeModelPackagePhase)

data KnowledgeModelPackageList = KnowledgeModelPackageList
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , description :: String
  , nonEditable :: Bool
  , public :: Bool
  , remoteVersion :: Maybe String
  , language :: String
  , createdAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance Ord KnowledgeModelPackageList where
  compare a b =
    compare a.id b.id
      <> compare a.version b.version
