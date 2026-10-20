module Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind

data KnowledgeModelEntityNodeDTO = KnowledgeModelEntityNodeDTO
  { uuid :: U.UUID
  , parentUuid :: Maybe U.UUID
  , entityType :: EntityKind
  , title :: String
  , depth :: Int
  , content :: Maybe KnowledgeModelEntity
  }
  deriving (Show, Eq, Generic)
