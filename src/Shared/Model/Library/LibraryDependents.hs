module Shared.Model.Library.LibraryDependents where

import qualified Data.UUID as U
import GHC.Generics

data LibraryDependents = LibraryDependents
  { knowledgeModelPackages :: [LibraryDependentKnowledgeModelPackage]
  , editors :: [LibraryDependentResource]
  , projects :: [LibraryDependentResource]
  , documents :: [LibraryDependentResource]
  , hidden :: LibraryHiddenDependents
  , deleteAllowed :: Bool
  }
  deriving (Show, Eq, Generic)

data LibraryDependentKnowledgeModelPackage = LibraryDependentKnowledgeModelPackage
  { uuid :: U.UUID
  , pId :: String
  , name :: String
  , version :: String
  }
  deriving (Show, Eq, Generic)

data LibraryDependentResource = LibraryDependentResource
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Show, Eq, Generic)

data LibraryHiddenDependents = LibraryHiddenDependents
  { knowledgeModelPackages :: Int
  , editors :: Int
  , projects :: Int
  , documents :: Int
  , workspaces :: Int
  }
  deriving (Show, Eq, Generic)

data LibraryDependentEntity
  = PackageLibraryDependentEntity
  | EditorLibraryDependentEntity
  | ProjectLibraryDependentEntity
  | DocumentLibraryDependentEntity
  deriving (Show, Eq, Generic, Read)

data LibraryDependent = LibraryDependent
  { entity :: LibraryDependentEntity
  , uuid :: U.UUID
  , name :: String
  , pId :: Maybe String
  , version :: Maybe String
  , workspaceUuid :: Maybe U.UUID
  , visible :: Bool
  }
  deriving (Show, Eq, Generic)
