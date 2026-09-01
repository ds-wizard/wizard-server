module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM ()
import Shared.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import Shared.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Shared.Util.Aeson

instance FromJSON KnowledgeModelEditorList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorList where
  toJSON = genericToJSON jsonOptions
