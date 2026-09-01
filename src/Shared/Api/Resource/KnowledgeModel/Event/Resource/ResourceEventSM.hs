module Shared.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Resource.ResourceEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Resource.ResourceEvent
import Shared.Util.Swagger

instance ToSchema AddResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_rc1

instance ToSchema EditResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_rc1

instance ToSchema DeleteResourceCollectionEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_rc1

-- --------------------------------------------
instance ToSchema AddResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_rc1_rp1

instance ToSchema EditResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_rc1_rp1

instance ToSchema DeleteResourcePageEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_rc1_rp1
