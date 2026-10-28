module Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelPackageSuggestion = KnowledgeModelPackageSuggestion
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , description :: String
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Ord, Generic)
