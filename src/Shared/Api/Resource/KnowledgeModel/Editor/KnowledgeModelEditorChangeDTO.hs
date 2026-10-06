module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO where

import GHC.Generics

data KnowledgeModelEditorChangeDTO = KnowledgeModelEditorChangeDTO
  { name :: String
  , id :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , language :: String
  }
  deriving (Generic)
