module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM where

import Data.Aeson

import Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Shared.Util.Aeson

instance FromJSON KnowledgeModelEditorChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorChangeDTO where
  toJSON = genericToJSON jsonOptions
