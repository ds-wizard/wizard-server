module Shared.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.Integration.IntegrationEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.Util.Swagger

instance ToSchema AddIntegrationEvent

instance ToSchema AddApiIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_ir

instance ToSchema AddPluginIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_io

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema EditIntegrationEvent

instance ToSchema EditApiIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_ir

instance ToSchema EditPluginIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_io

-- --------------------------------------------
-- --------------------------------------------
instance ToSchema DeleteIntegrationEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_ir
