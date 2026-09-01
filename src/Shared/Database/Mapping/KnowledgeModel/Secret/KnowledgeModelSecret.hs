module Shared.Database.Mapping.KnowledgeModel.Secret.KnowledgeModelSecret where

import Database.PostgreSQL.Simple

import Shared.Model.KnowledgeModel.KnowledgeModelSecret

instance ToRow KnowledgeModelSecret

instance FromRow KnowledgeModelSecret
