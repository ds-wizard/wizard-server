module Shared.Api.Resource.KnowledgeModel.Event.Tag.TagEventSM where

import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Api.Resource.KnowledgeModel.Event.Tag.TagEventJM ()
import Shared.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.Model.KnowledgeModel.Event.Tag.TagEvent
import Shared.Util.Swagger

instance ToSchema AddTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" a_km1_tds

instance ToSchema EditTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" e_km1_tds

instance ToSchema DeleteTagEvent where
  declareNamedSchema = toSwaggerWithType "eventType" d_km1_tds
