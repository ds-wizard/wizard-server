module Shared.Service.KnowledgeModel.KnowledgeModelValidation where

import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Service.KnowledgeModel.KnowledgeModelService

validateKmValidity :: WizardRequestContextC s m => [KnowledgeModelEvent] -> Maybe U.UUID -> m ()
validateKmValidity events mPkgUuid = do
  _ <- compileKnowledgeModel events mPkgUuid []
  return ()
