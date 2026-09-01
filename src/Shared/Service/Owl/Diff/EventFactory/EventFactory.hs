module Shared.Service.Owl.Diff.EventFactory.EventFactory where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.UUID as U

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel

class EventFactory entity where
  createAddEvent :: MonadIO m => U.UUID -> entity -> m KnowledgeModelEvent
  createEditEvent :: MonadIO m => (KnowledgeModel, KnowledgeModel) -> U.UUID -> entity -> entity -> m (Maybe KnowledgeModelEvent)
  createDeleteEvent :: MonadIO m => U.UUID -> entity -> m KnowledgeModelEvent
