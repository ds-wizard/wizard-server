module Shared.Model.KnowledgeModel.KnowledgeModelSecret where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelSecret = KnowledgeModelSecret
  { uuid :: U.UUID
  , name :: String
  , value :: String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , workspaceUuid :: Maybe U.UUID
  }
  deriving (Show, Generic)

instance Eq KnowledgeModelSecret where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.value == b.value
      && a.tenantUuid == b.tenantUuid
      && a.workspaceUuid == b.workspaceUuid
