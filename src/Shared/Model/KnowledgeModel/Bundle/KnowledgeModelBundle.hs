module Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle where

import GHC.Generics

import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage

data KnowledgeModelBundle = KnowledgeModelBundle
  { id :: String
  , name :: String
  , version :: String
  , metamodelVersion :: Int
  , packages :: [KnowledgeModelBundlePackage]
  }
  deriving (Show, Eq, Generic)

instance CoordinateFactory KnowledgeModelBundle where
  createCoordinate p =
    Coordinate
      { id = p.id
      , version = p.version
      }
