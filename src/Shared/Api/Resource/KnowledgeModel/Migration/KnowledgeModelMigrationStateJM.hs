module Shared.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM where

import Data.Aeson

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Shared.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Shared.Util.Aeson

instance FromJSON KnowledgeModelMigrationState where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON KnowledgeModelMigrationState where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
