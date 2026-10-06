module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage

data KnowledgeModelPackageSimpleDTO = KnowledgeModelPackageSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , remoteLatestVersion :: Maybe String
  , description :: String
  , nonEditable :: Bool
  , public :: Bool
  , language :: String
  , createdAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
