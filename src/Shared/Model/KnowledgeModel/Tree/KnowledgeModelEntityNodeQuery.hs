module Shared.Model.KnowledgeModel.Tree.KnowledgeModelEntityNodeQuery where

import GHC.Generics

data KnowledgeModelEntityNodeQuery = KnowledgeModelEntityNodeQuery
  { depth :: Maybe Int
  , content :: Bool
  }
  deriving (Show, Eq, Generic)
