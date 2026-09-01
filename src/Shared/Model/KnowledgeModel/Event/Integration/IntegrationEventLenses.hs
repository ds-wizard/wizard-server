module Shared.Model.KnowledgeModel.Event.Integration.IntegrationEventLenses where

import Shared.Model.Common.Lens
import Shared.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance HasVariables' EditIntegrationEvent (EventField [String]) where
  getVariables (EditApiIntegrationEvent' e) = e.variables
  getVariables (EditPluginIntegrationEvent' e) = NothingChanged
  setVariables (EditApiIntegrationEvent' e) newValue = EditApiIntegrationEvent' $ e {variables = newValue}
  setVariables (EditPluginIntegrationEvent' e) newValue = EditPluginIntegrationEvent' e
