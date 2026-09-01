module Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventFieldSM where

import Data.Swagger

import Shared.Model.KnowledgeModel.Event.KnowledgeModelEventField

instance ToSchema a => ToSchema (EventField a) where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
