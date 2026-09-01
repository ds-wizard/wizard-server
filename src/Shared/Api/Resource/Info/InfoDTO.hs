module Shared.Api.Resource.Info.InfoDTO where

import Data.Time
import GHC.Generics

import Shared.Model.Component.Component

data InfoDTO = InfoDTO
  { name :: String
  , version :: String
  , builtAt :: UTCTime
  , components :: [Component]
  , metamodelVersions :: [InfoMetamodelVersionDTO]
  }
  deriving (Show, Eq, Generic)

data InfoMetamodelVersionDTO = InfoMetamodelVersionDTO
  { name :: String
  , version :: String
  }
  deriving (Show, Eq, Generic)
