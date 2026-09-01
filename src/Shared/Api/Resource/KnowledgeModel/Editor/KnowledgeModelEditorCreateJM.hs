module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Shared.Util.Aeson

instance FromJSON KnowledgeModelEditorCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorCreateDTO where
  toJSON = genericToJSON jsonOptions
