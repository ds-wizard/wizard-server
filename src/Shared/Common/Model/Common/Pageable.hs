module Shared.Common.Model.Common.Pageable where

import GHC.Generics

data Pageable = Pageable
  { page :: Maybe Int
  , size :: Maybe Int
  }
  deriving (Show, Eq, Generic)
