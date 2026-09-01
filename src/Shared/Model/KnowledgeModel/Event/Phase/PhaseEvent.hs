module Shared.Model.KnowledgeModel.Event.Phase.PhaseEvent where

import Data.Hashable
import GHC.Generics

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.Util.KnowledgeModel.Hashable ()

data AddPhaseEvent = AddPhaseEvent
  { title :: String
  , description :: Maybe String
  , annotations :: [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable AddPhaseEvent

data EditPhaseEvent = EditPhaseEvent
  { title :: EventField String
  , description :: EventField (Maybe String)
  , annotations :: EventField [MapEntry String String]
  }
  deriving (Show, Eq, Generic)

instance Hashable EditPhaseEvent

data DeletePhaseEvent = DeletePhaseEvent
  deriving (Show, Eq, Generic)

instance Hashable DeletePhaseEvent
