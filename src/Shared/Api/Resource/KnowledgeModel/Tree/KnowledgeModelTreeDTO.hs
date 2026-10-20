module Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelTreeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO

data KnowledgeModelTreeDTO = KnowledgeModelTreeDTO
  { knowledgeModelUuid :: U.UUID
  , entities :: [KnowledgeModelEntityNodeDTO]
  }
  deriving (Show, Eq, Generic)
