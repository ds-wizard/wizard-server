module RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelPackageSimpleDTO = KnowledgeModelPackageSimpleDTO
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , description :: String
  , language :: String
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
