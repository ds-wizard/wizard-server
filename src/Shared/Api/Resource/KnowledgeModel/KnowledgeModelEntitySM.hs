module Shared.Api.Resource.KnowledgeModel.KnowledgeModelEntitySM where

import Data.Aeson (toJSON)
import Data.Swagger

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelEntityJM
import Shared.Model.KnowledgeModel.KnowledgeModelEntity
import Shared.Model.KnowledgeModel.KnowledgeModelEntityKind

instance ToSchema EntityKind where
  declareNamedSchema _ = return $ NamedSchema (Just "EntityKind") (mempty {_schemaParamSchema = kindSchema})
    where
      kindSchema = mempty {_paramSchemaType = Just SwaggerString, _paramSchemaEnum = Just (fmap (toJSON . entityKindId) entityNodeKinds)}

instance ToSchema KnowledgeModelEntity where
  declareNamedSchema _ = return $ NamedSchema (Just "KnowledgeModelEntity") mempty
