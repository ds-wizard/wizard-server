module Shared.Model.KnowledgeModel.Tree.KnowledgeModelTreeQuery where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelTreeQuery = KnowledgeModelTreeQuery
  { entityUuid :: Maybe U.UUID
  , depth :: Maybe Int
  , tagUuids :: [U.UUID]
  , content :: Bool
  }
  deriving (Show, Eq, Generic)
