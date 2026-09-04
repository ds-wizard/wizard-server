module Shared.Common.Model.Common.Sort where

import GHC.Generics

data SortDirection
  = Ascending
  | Descending
  deriving (Show, Eq, Generic)

data Sort = Sort
  { by :: String
  , direction :: SortDirection
  }
  deriving (Show, Eq, Generic)
