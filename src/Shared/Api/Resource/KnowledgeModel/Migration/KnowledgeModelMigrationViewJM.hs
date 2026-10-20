module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationViewJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.Event.KnowledgeModelEditorEventsChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationViewDTO
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Api.Resource.KnowledgeModel.Tree.KnowledgeModelEntityNodeJM
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance ToJSON KnowledgeModelMigrationViewDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelMigrationViewDTO where
  jsonSchema =
    withDefinitions [knowledgeModelEntityNodeDefinition]
      . withProperties [("state", enumSchema ["Running", "Conflict", "Error", "Completed"])]
      . genericJsonSchema jsonOptions

instance ToJSON KnowledgeModelMigrationConflictDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema KnowledgeModelMigrationConflictDTO where
  jsonSchema = genericJsonSchema jsonOptions
