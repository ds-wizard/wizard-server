module Shared.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorMetadataDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelEditorMetadataDTO = KnowledgeModelEditorMetadataDTO
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  , language :: String
  , previousPackageUuid :: Maybe U.UUID
  , workspaceUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
