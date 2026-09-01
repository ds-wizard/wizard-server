module Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.Registry.RegistryOrganization

data KnowledgeModelPackageSimpleDTO = KnowledgeModelPackageSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , phase :: KnowledgeModelPackagePhase
  , remoteLatestVersion :: Maybe String
  , description :: String
  , organization :: Maybe RegistryOrganization
  , nonEditable :: Bool
  , public :: Bool
  , language :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
