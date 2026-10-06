module Shared.Model.DocumentTemplate.DocumentTemplateDraftList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateDraftList = DocumentTemplateDraftList
  { uuid :: U.UUID
  , name :: String
  , id :: String
  , version :: String
  , description :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance Ord DocumentTemplateDraftList where
  compare a b =
    compare a.id b.id
      <> compare a.version b.version
