module Shared.Model.KnowledgeModel.Event.Expert.ExpertEvent where

import Data.Hashable
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Util.KnowledgeModel.Hashable ()

data AddExpertEvent = AddExpertEvent
  { name :: String
  , email :: String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddExpertEvent

data EditExpertEvent = EditExpertEvent
  { name :: EventField String
  , email :: EventField String
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditExpertEvent

data DeleteExpertEvent = DeleteExpertEvent
  deriving (Show, Eq, Generic)

instance Hashable DeleteExpertEvent
